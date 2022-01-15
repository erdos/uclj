(ns uclj.core
  (:gen-class)
  (:import [clojure.lang Gateway Var LazySeq]
           [java.lang Number]
           [java.io StringWriter]
           [java.util.concurrent.atomic AtomicReferenceArray]))

(set! *warn-on-reflection* true)

(def namespaces-to-require
  '[[clojure.core]
    [clojure.core.async :as async]
                                        ;[clojure.core.logic :as logic]
                                        ;clojure.data
                                        ;clojure.datafy
    [clojure.data.csv :as csv]
    [clojure.data.json :as json]
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.data.xml :as xml]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.math.combinatorics :as combo]
    [clojure.pprint :as pprint :refer [pprint pp]]
    [clojure.set :as set]
    [clojure.string :as s]
    [clojure.test :refer [deftest testing is are]]
    [clojure.walk :as walk]
    [clojure.zip :as zip]])

(run! require namespaces-to-require)

(def ^:dynamic *template-vars* {})
(defn- template* [bindings expr]
  (assert (vector? bindings))
  (letfn [(unroll [expr] (cond (seq? expr) (unroll-seq expr)
                              (vector? expr) [(vec (mapcat unroll expr))]
                              :else [expr]))
          (evaling [expr]
            (eval `(let [~@(mapcat seq (for [[k] *template-vars*] [k `(*template-vars* '~k)]))] ~expr)))
          (unroll-seq [[t s :as expr]]
            (cond ('#{clojure.core/unquote-splicing} t) (evaling s)
                  ('#{clojure.core/unquote} t)          [(evaling s)]
                  :else                                 [(doall (mapcat unroll expr))]))]
    (binding [*template-vars* (eval `(let [~@bindings] ~(into {} (for [s (flatten (take-nth 2 bindings))] [(list 'quote s) s]))))]
      (first (unroll expr)))))

;; Works like an inlined defmacro call: The expression in (template) calls will be the result of the macro expansion.
;; You can include unquoted (ie.:  ~() and ~@() ) forms in the expression so the evaluation result will be substituted
;; in their place. Optionally, a binding vector can be given as a first argument, this will bind values to symbols that
;; can be used when evaluating unquoted subexpressions.
(defmacro template
  ([vars expr] (template* vars expr))
  ([expr] (template* [] expr)))

;; Maps f over all elements of coll. Does not change type of collection.
(defn- map-coll [f coll]
  (into (empty coll) (map (if (map? coll) (fn [[a b]] [(f a) (f b)]) f)) coll))

;; resolves to class or nil or throws exception
(defn symbol->class [sym]
  (assert (symbol? sym))
  (doto (ns-resolve *ns* sym)
    (some-> (-> class? (assert (str "Cannot resolve to class " sym))))))

(declare seq->eval-node)
(defn- special-form? [s] (contains? (methods seq->eval-node) s))

(defn- add-env [&env sym]
  ;; to ensure content of &env will be symbol->LocalBinding mapping
  (assert (map? &env))
  (assert (symbol? sym) (str "Not symbol: " (pr-str sym) " of " (type sym)))
  (let [local-binding (Gateway/localBinding 0 sym nil false)]
    (assoc &env sym local-binding)))

(defn- macro? [sym]
  (when (symbol? sym)
    (when-let [r (resolve sym)]
      (.isMacro ^clojure.lang.Var r))))

(defn- expand-macro [&env [sym & args :as form]]
  (apply (resolve sym) form &env args))

(defn- macroexpand-1-code [&env exp]
  ; (println :env &env exp)
  (cond (and (seq? exp) (special-form? (first exp))) exp

        ;; (Type/staticmethod 1 2 3) -> (. Type staticmethod 1 2 3)
        (and (seq? exp)
             (symbol? (first exp))
             (namespace (first exp))
             (symbol->class (symbol (namespace (first exp)))))
        (list* '.
                (symbol (namespace (first exp)))
                (symbol (name (first exp)))
                (next exp))

        ;; (.method x 1 2 3) -> (. x method 1 2 3)
        (and (seq? exp)
             (symbol? (first exp))
             (.startsWith (name (first exp)) ".")
             (not (#{"." ".."} (name (first exp)))))
        (list* '.
                (second exp)
                (symbol (namespace (first exp)) (.substring (name (first exp)) 1))
                (nnext exp))

        ;; (ctor. 1 2 3) -> (new ctor 1 2 3)
        (and (seq? exp)
             (symbol? (first exp))
             (.endsWith (name (first exp)) ".")
             (not (#{"." ".."} (name (first exp)))))
        (list* 'new
                (symbol (namespace (first exp))
                        (.substring (name (first exp)) 0 (dec (count (name (first exp))))))
                (next exp))

        (and (seq? exp) (macro? (first exp)))        (expand-macro &env exp)
        :else                                        exp))

(defn- macroexpand-code [&env exp]
  (let [e (macroexpand-1-code &env exp)]
    (if (identical? e exp)
      exp
      (recur &env e))))

;; return seq of (fn-name ([args*] bodies*)+)
(defn- parsed-fn [[_ & bodies]]
  (let [fname (when (symbol? (first bodies)) (first bodies))
        bodies (if fname (next bodies) bodies)
        bodies (if (vector? (first bodies)) (list bodies) bodies)]
    (cons fname bodies)))

(defn macroexpand-all-code [exp]
  ((fn iter [&env exp]
     (let [expanded (macroexpand-code &env exp)]
       (cond
         (seq? expanded)
         (case (first expanded)
           quote expanded

           letfn*
           (let [&env (reduce add-env &env (take-nth 2 (second expanded)))]
             (map (partial iter &env) expanded))

           def
           (list* 'def (with-meta (second expanded) (iter &env (meta (second expanded))))
                       (map (partial iter &env) (nnext expanded)))

           (loop* let*)
           (let [_ (assert (vector? (second expanded)))
                 [bindings-vec &env]
                 (reduce (fn r [[bindings-vec &env] [k v]]
                           (assert (map? &env))
                           [(conj bindings-vec k (iter &env v))
                            (add-env &env k)])
                         [[] &env]
                         (partition 2 (second expanded)))]
             (list* (first expanded)
                    bindings-vec
                    (map (partial iter &env) (nnext expanded))))

           fn*
           (let [[fname & bodies] (parsed-fn expanded)
                 &env             (if fname (add-env &env fname) &env)]
             (concat '[fn*]
                     (when fname [fname])
                     (for [[args & bodies] bodies
                           :let [&env (reduce add-env &env args)]]
                       (list* args (map (partial iter &env) bodies)))))

           ;; else
           (map (partial iter &env) expanded))

         (or (vector? expanded) (set? expanded) (map? expanded))
         (map-coll #(iter &env %) expanded)

         :else ;; scalars
         expanded)))
   {} exp))

(defprotocol EvalNode
  (evalme [this #^objects bindings]))

;; These types evaluate to themselves so it would be redundant wrap them in an reified EvalNode instance.
(extend-protocol EvalNode
  nil                            (evalme [_ _] nil)
  Number                         (evalme [t _] t)
  Boolean                        (evalme [t _] t)
  Character                      (evalme [t _] t)
  String                         (evalme [t _] t)
  java.util.regex.Pattern        (evalme [t _] t)
  clojure.lang.Keyword           (evalme [t _] t))

;; TODO: test with interfaces instead of protocols!
(defmacro gen-eval-node
  ([m body] `(with-meta (gen-eval-node ~body) ~m))
  ([body] `(reify EvalNode (evalme [_ ~'&b] (let [~(quote #^objects &b) ~'&b] ~body)))))

(declare ->eval-node)

;; &a is a hash-map of identity-symbol to index in local bindings.
(defmulti seq->eval-node (fn [iden->idx recur-indices form] (first form))
                         :default ::default)

;; return list of lists
(defn- var->arglists [v]
  (assert (var? v))
  (when (fn? @v)
    (seq (for [args (:arglists (meta v))]
           (if (= '& (last (butlast args)))
             (concat (repeatedly (- (count args) 2) gensym) '[& variadic])
             (repeatedly (count args) gensym))))))

(def clojure-core-inlined-fns
  (template
   (hash-map
    ~@(mapcat seq
              (for [ns    namespaces-to-require
                    :let  [ns (if (vector? ns) (first ns) ns)]
                    v     (vals (ns-publics ns))
                    :when (not (#{#'clojure.core/aget #'clojure.core/aclone ;; suppress reflection warning
                                  #'clojure.core/alength #'clojure.core/aset} v))
                    :when (not (:macro (meta v)))
                    :let [arglists (var->arglists v)]
                    :when arglists]
                [v (list* 'fn*
                           (symbol (str (name (symbol v)) "-inlined"))
                           (for [args arglists]
                             (if (= 'variadic (last args))
                               (list (vec args) (list 'gen-eval-node (concat ['clojure.core/apply (symbol v)]
                                                                             (for [a (butlast (butlast args))] (list 'evalme a '&b))
                                                                             [(list 'clojure.core/for [(last args) (last args)] (list 'evalme (last args) '&b))])))
                               (list (vec args) (list 'gen-eval-node (list* (symbol v) (for [a args] (list 'evalme a '&b))))))))])))))

(defmethod seq->eval-node ::default seq-eval-call [&a _ s]
  (if (empty? s)
    (gen-eval-node ())
    (let [[f & args] (map (partial ->eval-node &a nil) s)
          [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15] args] ;; TODO: unroll with template!
      (dorun args)
      (if-let [call-factory (clojure-core-inlined-fns (::var (meta f)))]
        (apply call-factory args)
        (template [a-symbol #(symbol (str 'a %))]
          (case (count args)
            ~@(for [i (range 16)]
                [i (list 'gen-eval-node (list* '.invoke (quote ^clojure.lang.IFn (evalme f &b))
                                                (for [j (range 1 (inc i))] (list 'evalme (a-symbol j) '&b))))])
            ;; else
            (gen-eval-node (apply (evalme f &b) (for [e args] (evalme e &b))))))))))

(defmethod seq->eval-node 'quote seq-eval-quote [_ _ [_ quoted]] (gen-eval-node quoted))

(defmethod seq->eval-node 'if seq-eval-if [&a recur-indices [_ condition then else]]
  (let [condition (->eval-node &a recur-indices condition)
        then      (->eval-node &a recur-indices then)
        else      (->eval-node &a recur-indices else)]
    (if else ;; shortcut: if else branch is missing then no need to call
      (gen-eval-node (if (evalme condition &b) (evalme then &b) (evalme else &b)))
      (gen-eval-node (if (evalme condition &b) (evalme then &b))))))

(defmethod seq->eval-node 'def seq-eval-def [&a _ [_ def-name & def-bodies]]
  (let [[docstring def-value] (if (= 2 (count def-bodies))
                                def-bodies
                                [nil (first def-bodies)])
        var-object ^clojure.lang.Var (intern *ns* def-name)
        value-node (->eval-node &a nil def-value)
        var-meta   (if docstring (assoc (meta def-name) :doc docstring) (meta def-name))
        meta-nodes (map-coll #(->eval-node &a nil %) var-meta)]
    (gen-eval-node
     (let [m (map-coll #(evalme % &b) meta-nodes)]
       (.setMeta var-object m)
       (when (:dynamic m)
         (.setDynamic var-object))
       (when (not-empty def-bodies)
         (.bindRoot var-object (evalme value-node &b)))
       var-object))))

(defmethod seq->eval-node 'case* seq-eval-case
  [&a recur-indices [_ value shift mask default-value imap switch-type mode skip-check :as form]]
  ;; (case* ~ge ~shift ~mask ~default ~imap ~switch-type :int)
  ;; switch-type: sparse vs compact
  ;; mode: ints hashes identity
  ;; - ints: if all keys are integers between MIN and MAX values
  ;; - hash-identity: if all keys are keywords
  ;; - hash-equiv: objects
  (let [imap (zipmap (keys imap)
                     (for [[h branch] (vals imap)]
                       [h (->eval-node &a recur-indices branch)]))
        default-value (->eval-node &a recur-indices default-value)
        value-node    (->eval-node &a recur-indices value)]
    (gen-eval-node
     (let [ev (evalme value-node &b)
           eh (clojure.lang.Util/hash ev)
           th (if (zero? mask) eh (bit-and (bit-shift-right eh shift) mask))]
       (if-let [[h branch] (get imap th)]
         (if (and skip-check (not (skip-check th)))
           (if (= eh h)
             (evalme branch &b)
             (evalme default-value &b))
           (evalme branch &b))
         (evalme default-value &b))))))

(defmethod seq->eval-node 'do seq-eval-do [iden->idx recur-indices [_ & bodies]]
  (let [bodies (concat (map (partial ->eval-node iden->idx nil) (butlast bodies))
                       (list (->eval-node iden->idx recur-indices (last bodies))))
        [b1 b2 b3 b4 b5 b6 b7 b8 b9] bodies]
    (template [b-symbol #(symbol (str 'b %))]
     (case (count bodies)
       0 nil
       1 (first bodies)
       ~@(mapcat seq
                 (for [i (range 2 10)]
                   [i (list 'gen-eval-node
                             (list* 'do (for [j (range 1 (inc i))]
                                          (list 'evalme (b-symbol j) '&b))))]))
       ;; else
       (let [butlast-body (doall (butlast bodies))
             last-body    (last bodies)]
         (gen-eval-node
          (do (doseq [x butlast-body] (evalme x &b))
              (evalme last-body &b))))))))

(defmethod seq->eval-node 'letfn* seq-eval-letfn [iden->idx recur-indices [_ bindings & bodies :as form]]
  (let [promises (for [[k f] (partition 2 bindings)
                       :let [v (volatile! nil)]]
                   [(-> k meta ::symbol-identity iden->idx int)
                    (->eval-node iden->idx nil f)
                    (fn [x] (vreset! v x))
                    (fn [& args] (apply @v args))])
        body-node (->eval-node iden->idx recur-indices (list* 'do bodies))]
    (gen-eval-node
     (do (doseq [[idx _ _ afn] promises]
           (aset &b idx afn))
         (doseq [[_ node deliver _] promises]
           (deliver (evalme node &b)))
         (evalme body-node &b)))))

(def ^:private kvs-seq (repeatedly #(vector (gensym "k") (gensym "v"))))

;; max-arity: [0-20)
(defmacro ^:private make-fn-body-upto-arity [max-arity fname symbol-used new-idx->old-idx arity->body-node arity->symbols-introduced]
  (assert (integer? max-arity))
  (assert (symbol? symbol-used))
  `(let [~'enclosed-array-size                                                  (int (if ~fname (inc (count ~symbol-used)) (count ~symbol-used)))
         body-vararg#                                                           (:variadic ~arity->body-node)
         body-vararg-symbols#                                                   (:variadic ~arity->symbols-introduced)
         [~@(for [i (range (inc max-arity))] (symbol (str 'body i)))]           (map ~arity->body-node (range))
         [~@(for [i (range (inc max-arity))] (symbol (str 'body i '-symbols)))] (map ~arity->symbols-introduced (range))]
    (gen-eval-node
        ;; the enclosed-array contains enclosed context
        (let [~'enclosed-array (object-array ~'enclosed-array-size)]
          (reduce-kv (fn [_# new-idx# old-idx#] (aset ~'enclosed-array new-idx# (aget ~'&b old-idx#))) nil ~new-idx->old-idx)
          (doto (fn ~@(for [i (range (inc max-arity))
                            :let [arg-symbols (repeatedly i gensym)]]
                        (list (vec arg-symbols)
                              `(assert ~(symbol (str 'body i '-symbols)) "Called with too many parameters!")
                              `(let [~'invocation-array (java.util.Arrays/copyOf
                                                          ~'enclosed-array (+ (count ~(symbol (str 'body i '-symbols))) ~'enclosed-array-size))]
                                  ~@(for [j (range i)]
                                      (list 'aset 'invocation-array (list '+ j 'enclosed-array-size) (nth arg-symbols j)))
                                    (loop []
                                      (let [result# (evalme ~(symbol (str 'body i)) ~'invocation-array)]
                                        (if (identical? ::recur result#)
                                          (recur)
                                          result#))))))
                   ([~@(for [i (range max-arity)] (symbol (str 'arg- i))) ~'& arg-rest#]
                       (assert body-vararg-symbols# "Called with too many parameters!")
                       (let [~'invocation-array (java.util.Arrays/copyOf ~'enclosed-array (+ (count body-vararg-symbols#) ~'enclosed-array-size))]
                          ~@(for [j (range (+ max-arity))]
                              (list 'aset 'invocation-array (list '+ j 'enclosed-array-size) (symbol (str 'arg- j))))
                            (aset ~'invocation-array (+ ~max-arity ~'enclosed-array-size) arg-rest#)
                            (loop []
                              (let [result# (evalme body-vararg# ~'invocation-array)]
                                (if (identical? ::recur result#)
                                  (recur)
                                  result#))))))
            (cond->> ~fname (aset #^objects ~'enclosed-array (dec ~'enclosed-array-size))))))))


(defn- make-fn-body [fname symbol-used arity->body-node arity->symbols-introduced iden->idx vararg-arity]
  (let [new-idx->old-idx (not-empty (mapv iden->idx symbol-used))]
    (template
      (case (int (apply max (or vararg-arity 0) (keys (dissoc arity->body-node :variadic))))
        ~@(mapcat seq (for [i (range 19)]
                         [i (list 'do (list 'make-fn-body-upto-arity i 'fname 'symbol-used 'new-idx->old-idx 'arity->body-node 'arity->symbols-introduced))]))))))

(defmethod seq->eval-node 'fn* seq-eval-fn [iden->idx recur-indices form]
  (assert (meta form))
  (let [[fname & bodies]          (parsed-fn form)
        symbol-used               (::symbol-used (meta form)) ;; set of all lexical bindings enclosed in the fn
        arity->def                (reduce (fn [m [args & bodies :as def]] (assoc m (if (some #{'&} args) :variadic (count args)) def)) {} bodies)
        arity->symbols-introduced (into {} (for [[k v] arity->def] [k (::fn-sym-introduced (meta v))]))
        vararg-arity              (when-let [[args] (arity->def :variadic)] (- (count args) 2))
        arity->body-node          (into {}
                                        (for [[arity [args & bodies :as def]] arity->def
                                              :let [iden->idx (zipmap (concat symbol-used
                                                                              (when-let [sym (::fn-sym-own (meta def))] [sym])
                                                                              (::fn-sym-introduced (meta def)))
                                                                      (range))
                                                    recur-indices (mapv iden->idx (::symbol-loop (meta def)))]]
                                          [arity (->eval-node iden->idx recur-indices (list* 'do bodies))]))]
    (make-fn-body fname symbol-used arity->body-node arity->symbols-introduced iden->idx vararg-arity)))

(def ^:const max-let-bindings 32)

(defmethod seq->eval-node 'let* seq-eval-let [iden->idx recur-indices [_ bindings & bodies :as form]]
  (cond
    ;; can merge (let*) forms
    (and (= 1 (count bodies))
         (< (count bindings) (* 2 max-let-bindings))
         (seq? (first bodies)) (= 'let* (ffirst bodies)))
    (recur iden->idx recur-indices (list* 'let*
                                          (into bindings (second (first bodies)))
                                          (nnext (first bodies))))
    ;; it must have at most max-let-bindings binding vars
    (> (count bindings) (* 2 max-let-bindings))
    (recur iden->idx recur-indices
           (list 'let*
                 (vec (take (* 2 max-let-bindings) bindings))
                 (list* 'let*
                        (vec (drop (* 2 max-let-bindings) bindings))
                        bodies)))
    :else
    (template [idx-symbols  (mapv #(symbol (str 'idx- %)) (range max-let-bindings))
               node-symbols (mapv #(symbol (str 'node- %)) (range max-let-bindings))]
      (let [[~@(map vector idx-symbols node-symbols)]
            (for [[k v] (partition 2 bindings)]
              [(int (iden->idx (::symbol-identity (meta k))))
              (->eval-node iden->idx nil v)])
            body-node (seq->eval-node iden->idx recur-indices (list* 'do bodies))]
        (case (count bindings)
          ~@(mapcat seq
                    (for [i (range (inc max-let-bindings))]
                      [(* 2 i)
                        `(gen-eval-node
                          (do ~@(for [i (range i)]
                                  `(aset ~'&b ~(idx-symbols i) (evalme ~(node-symbols i) ~'&b )))
                              (evalme ~'body-node ~'&b)))])))))))

(def ^:const max-loop-bindings 24)
(defmethod seq->eval-node 'loop* seq-eval-loop [iden->idx _ [_ bindings & bodies :as def]]
  (assert (even? (count bindings)))
  (assert (vector? (::symbol-loop (meta def))))
  (assert (every? symbol? (::symbol-loop (meta def))))
  (let [bindings-nodes                  (for [[_ v] (partition 2 bindings)] (->eval-node iden->idx nil v))
        recur-indices                   (mapv iden->idx (::symbol-loop (meta def)))
        body-node                       (seq->eval-node iden->idx recur-indices (list* 'do bodies))]
    (template [idx-symbols  (mapv #(symbol (str 'idx %)) (range max-loop-bindings))
               node-symbols (mapv #(symbol (str 'node %)) (range max-loop-bindings))]
      (case (count bindings)
        ~@(mapcat seq
                  (for [i (range (inc max-loop-bindings))]
                    [(* 2 i)
                     `(let [[~@(take i node-symbols)] ~'bindings-nodes
                            [~@(take i idx-symbols)]  ~'recur-indices]
                        (gen-eval-node
                         (do ~@(for [i (range i)]
                                 `(aset ~'&b ~(idx-symbols i) (evalme ~(node-symbols i) ~'&b )))
                             (loop []
                               (let [result# (evalme ~'body-node ~'&b)]
                                 (if (identical? ::recur result#)
                                   (recur)
                                   result#))))))]))))))

(defmethod seq->eval-node 'new seq-eval-new [&a recur-indices [_ class-name & args]]
  (let [clz (symbol->class class-name)
        _   (assert clz (str "Unexpected class name: " class-name))
        [arg0 :as args] (map (partial ->eval-node &a nil) args)]
    (case class-name
      ;; inline direct calls
      clojure.lang.LazySeq (gen-eval-node (new clojure.lang.LazySeq (evalme arg0 &b)))

      ;; else
      (gen-eval-node
       (let [args (for [a args] (evalme a &b))]
         (clojure.lang.Reflector/invokeConstructor clz (into-array Object args)))))))

(def ^:private node-symbols (for [i (range)] (symbol (str 'node- i))))
(def ^:private index-symbols (for [i (range)] (symbol (str 'index i))))

(defmethod seq->eval-node 'recur seq-eval-recur [iden->idx recur-indices [_ & values]]
  (when-not recur-indices
    (throw (new UnsupportedOperationException "Can only recur from tail position")))
  (when-not (= (count recur-indices) (count values))
    (throw (new IllegalArgumentException (str "Mismatched argument count to recur, expected: " (count recur-indices) " args, got: " (count values)))))
  (template
    (case (count ~'recur-indices)
      ~@(mapcat seq
          (for [i (range 20)
                :let [node-symbols  (take i node-symbols)
                      index-symbols (take i index-symbols)]]
            [i
              `(let [[~@node-symbols]  (map (partial ->eval-node ~'iden->idx nil) ~'values)
                     [~@index-symbols] ~'recur-indices]
                 (gen-eval-node
                   (let [~@(interleave node-symbols (for [n node-symbols] (list 'evalme n '&b)))]
                     ~@(map (partial list 'aset '&b) index-symbols node-symbols)
                     ::recur)))])))))

(defmethod seq->eval-node 'throw [&a _ [_ e :as form]]
  (assert (= 2 (count form)))
  (assert (or (symbol? e) (seq? e)))
  (let [e (->eval-node &a nil e)]
    (gen-eval-node (throw (evalme e &b)))))

(defmethod seq->eval-node 'clojure.core/eval [&a _ [_ e]]
  (let [e (->eval-node &a nil e)]
    (gen-eval-node (-> e (evalme &b) (evalme nil)))))

(defmethod seq->eval-node 'var [&a _ [_ v]]
  (let [x (resolve v)]
    (assert x (str "Unable to resolve var: " (pr-str v) " in this context in ns " *ns*))
    (gen-eval-node x)))

(defmethod seq->eval-node '. [&a _ [_ target field & args]]
  (let [[field args] (if (seq? field) [(first field) (next field)] [field args])]
    (let [[arg1 arg2 arg3 :as args] (map (partial ->eval-node &a nil) args)]
      (if-let [target-class (cond (class? target) target
                                  (and (symbol? target) (class? (resolve target))) (resolve target))]
        ;; static invocation
        (gen-eval-node
         (clojure.lang.Reflector/invokeStaticMethod
          ^Class target-class
          (name field)
          ^objects (into-array Object (for [a args] (evalme a &b)))))

        ;; instance invocation
        (let [target (->eval-node &a nil target)]
          (case field
            nth ;; very common in let* forms
            (gen-eval-node (nth (evalme target &b) (evalme arg1 &b) (evalme arg2 &b)))

            equals
            (gen-eval-node (.equals ^Object (evalme target &b) (evalme arg1 &b)))

            ;; else
            (gen-eval-node
             (clojure.lang.Reflector/invokeInstanceMethod
              (evalme target &b) (name field) (into-array Object (for [a args] (evalme a &b)))))))))))

;; We need a way to hard reset a value bound to a Var object.
;; For some reason, Var.doSet works in the Clojure REPL but breaks when compiled with GraalVM.
;; And Var.doReset works only in GraalVM. So we choose to use one over another depending on environemnt.
(defn- var-set-reset! [^clojure.lang.Var v new-val]
  (try
    ;; works in Clojure REPL but breaks with GraalVM:
    (.doSet v new-val)
    (catch IllegalStateException _
      ;; runs in GraalVM but does nothing in REPL:
      (.doReset v new-val))))

(doseq [lf '[in-ns clojure.core/in-ns]]
  (defmethod seq->eval-node lf [&a _ [_ nssym]]
    (let [sym-node (->eval-node &a nil nssym)]
      (gen-eval-node
       (let [new-ns (create-ns (evalme sym-node &b))]
         (alter-meta! (var *ns*) assoc :dynamic true)
         (var-set-reset! (var *ns*) new-ns))))))

(declare evaluator)
(doseq [lf '[clojure.core/load-file load-file]]
  (defmethod seq->eval-node lf [&a _ [_ fname]]
    (let [arg (->eval-node &a nil fname)]
      (gen-eval-node
       (with-open [in (new java.io.PushbackReader (io/reader (io/file (evalme arg &b))))]
         (doseq [read (repeatedly #(read {:eof ::eof} in))
                 :while (not= ::eof read)]
           (evaluator read)))))))

;; Needed by (ns) forms.
(defmethod seq->eval-node 'clojure.core/with-loading-context [&a _ [_ & bodies]]
  (seq->eval-node &a nil (list* 'do bodies)))

(defmethod seq->eval-node 'try [iden->idx recur-indices [_ & xs]]
  (let [catch-clauses  (keep (fn [x] (when (and (seq? x) (= 'catch (first x)))
                                       [(resolve (nth x 1)) ;; type
                                        (-> (nth x 2) meta ::symbol-identity iden->idx)
                                        (->eval-node iden->idx recur-indices (list* 'do (nthrest x 2)))]))
                             xs)
        bodies         (remove (fn [x] (and (seq? x) ('#{finally catch} (first x)))) xs)
        body-node      (seq->eval-node iden->idx recur-indices (list* 'do bodies))
        finally-node (some (fn [x] (when (and (seq? x) (= 'finally (first x)))
                                     (->eval-node iden->idx nil (list* 'do (next x))))) xs)]
    (gen-eval-node
     (try (evalme body-node &b)
          (catch Throwable t
            (if-let [[_ idx node] (some (fn [[c v node :as item]] (when (instance? c t) item)) catch-clauses)]
              (do (aset #^objects &b idx t)
                  (evalme node &b))
              (throw t)))
          (finally (evalme finally-node &b))))))

(defn sym->eval-node [iden->idx expr]
  (if-let [identity (::symbol-identity (meta expr))]
    (let [index (int (iden->idx identity))]
      (gen-eval-node (aget #^objects &b index)))
    (if (var? (resolve expr))
      (let [^clojure.lang.Var resolved-expr (resolve expr)]
        (if (and (bound? resolved-expr) (not (.isDynamic resolved-expr)))
          (let [expr-val @resolved-expr] (gen-eval-node {::var resolved-expr} expr-val))
          (gen-eval-node {::unbound-var resolved-expr} @resolved-expr))) ;; var was unbound at compile time so we need to deref in in runtime
      (if-let [parent (some-> expr namespace symbol resolve)]
        (if (class? parent)
          (gen-eval-node (clojure.lang.Reflector/getStaticField ^Class parent (name expr)))
          (throw (ex-info (str "Cannot access symbol! " expr) {:symbol expr})))
        (throw (ex-info (str "Cannot resolve symbol! " expr) {:symbol expr}))))))

(defn- coll->eval-node [fmap sym->iden expr]
  (let [elem-nodes (into [] (fmap (partial ->eval-node sym->iden nil)) expr)
        init       (empty expr)]
    (if-let [meta-node (->eval-node sym->iden nil (::meta-exp (meta expr)))]
      (gen-eval-node (with-meta (into init (fmap #(evalme % &b)) elem-nodes)
                                (evalme meta-node &b)))
      (gen-eval-node (into init (fmap #(evalme % &b)) elem-nodes)))))

(defn ->eval-node [sym->iden recur-indices expr]
  (when recur-indices
    (assert (every? integer? recur-indices)))
  (cond (seq? expr)    (seq->eval-node sym->iden recur-indices expr)
        (map? expr)    (coll->eval-node #(map (fn [[a b]] [(% a) (% b)])) sym->iden expr)
        (set? expr)    (coll->eval-node map sym->iden expr)
        (vector? expr) (coll->eval-node map sym->iden expr)
        (symbol? expr) (sym->eval-node sym->iden expr)
        :else          expr))

;; Recursively walks macroexpanded code and adds meta information about var usage.
;; First arg: map from symbol to identity symbol.
;; The metadata of response will have these keys:
;;  ::symbol-introduced - (identity) vars in let bindings
;;  ::symbol-used - (identity) vars that are bound from outside of current context
;; For symbols: ::symbol-identity - the (identity) symbol that can be used for evaluating
(defmulti enhance-code (fn [sym->iden e] (if (seq? e) (first e) (type e))) :default ::default)

(defmethod enhance-code ::default [sym->iden v]
  (if (seq? v)
    ;; method calls
    (let [bodies (doall (for [b v] (enhance-code sym->iden b)))]
      (with-meta bodies {::symbol-used       (set (mapcat (comp ::symbol-used meta) bodies))
                         ::symbol-introduced (set (mapcat (comp ::symbol-introduced meta) bodies))}))
    ;; scalar values: string, numbers, etc.
    v))

;; (defmethod enhance-code 'case* [sym->iden [_ value shift mask default-value imap switch-type mode skip-check]])

(defmethod enhance-code clojure.lang.Symbol [sym->iden s]
  (if-let [iden (sym->iden s)]
    (with-meta s {::symbol-identity iden, ::symbol-used #{iden}})
    s))

;; For vector, set, map: recursively run function on all elems and also on meta map.
(doseq [[t                               coll->elems          elems->coll]
        [[clojure.lang.IPersistentVector seq                  vec        ]
         [clojure.lang.IPersistentSet    seq                  set        ]
         [clojure.lang.IPersistentMap    (partial mapcat seq) (partial apply hash-map)]]]
  (defmethod enhance-code t [sym->iden coll]
    (let [enhanced-meta (enhance-code sym->iden (meta coll))
          elems         (for [c (coll->elems coll)] (enhance-code sym->iden c))]
      (with-meta (elems->coll elems)
        {::meta-exp          enhanced-meta
         ::symbol-used       (set/union (set (mapcat (comp ::symbol-used meta) elems))
                                        (::symbol-used (meta enhanced-meta)))
         ::symbol-introduced (set/union (set (mapcat (comp ::symbol-introduced meta) elems))
                                        (::symbol-introduced (meta enhanced-meta)))}))))

(doseq [t '[let* loop*]]
  (defmethod enhance-code t [sym->iden [form bindings & bodies]]
    (let [[sym->iden introduced-idents bindings]
          (reduce (fn [[sym->iden introduced-idents mapped-bindings] [k v]]
                    (let [key-iden (gensym)]
                      [(assoc sym->iden k key-iden)
                       (conj introduced-idents key-iden)
                       (conj mapped-bindings
                             (with-meta k {::symbol-identity key-iden})
                             (enhance-code sym->iden v))]))
                  [sym->iden #{} []]
                  (partition 2 bindings))
          bodies      (for [body bodies] (enhance-code sym->iden body))
          symbol-used (set (remove introduced-idents
                                   (mapcat (comp ::symbol-used meta)
                                           (concat bodies (take-nth 2 (next bindings))))))
          symbol-introduced (-> introduced-idents
                                (into (mapcat (comp ::symbol-introduced meta) bodies))
                                (into (mapcat (comp ::symbol-introduced meta) bindings)))]
      (with-meta (list* form bindings bodies)
        {::symbol-used       symbol-used
         ::symbol-introduced symbol-introduced
         ::symbol-loop       (when (= form 'loop*) (mapv sym->iden (take-nth 2 bindings)))}))))

(defmethod enhance-code 'def [sym->iden [_ var-sym & tail]]
  (let [var-sym   (vary-meta var-sym (partial reduce-kv (fn [m k v] (assoc m (enhance-code sym->iden k) (enhance-code sym->iden v))) {}))
        tail      (map (partial enhance-code sym->iden) tail)
        all-metas (map meta (cons (last tail) (mapcat seq (meta var-sym))))]
    (with-meta (list* 'def var-sym tail)
      {::symbol-used       (set (mapcat ::symbol-used       all-metas))
       ::symbol-introduced (set (mapcat ::symbol-introduced all-metas))})))

(defmethod enhance-code 'fn* [sym->iden fn-expression]
  (let [[fname & fbodies] (parsed-fn fn-expression)
        fbodies (for [[args & bodies] fbodies
                      :let [arg-syms  (remove #{'&} args)
                            new-acc   (cond-> (zipmap arg-syms (repeatedly gensym))
                                        fname (assoc fname (gensym)))
                            new-acc-1 (zipmap (vals new-acc) (keys new-acc))
                            sym->iden (merge sym->iden new-acc)
                            bodies    (for [body bodies] (enhance-code sym->iden body))
                            args      (mapv (fn [s] (with-meta s {::symbol-identity (new-acc s)})) args)
                            symbol-loop (mapv new-acc arg-syms)]]
                  (with-meta (list* args bodies)
                    {::symbol-used       (set (remove new-acc-1 (mapcat (comp ::symbol-used meta) bodies)))
                     ::fn-sym-own        (new-acc fname)
                     ::fn-sym-introduced (-> symbol-loop ;; arguments + let vars
                                             (into (set (mapcat (comp ::symbol-introduced meta) bodies))))
                     ::symbol-loop       symbol-loop
                     #_(set (keys new-acc-1))}))]
    (with-meta (if fname (list* 'fn* fname fbodies) (list* 'fn* fbodies))
      ;; symbol-introduced is nil because it is a new closure!
      {::symbol-used (set (mapcat (comp ::symbol-used meta) fbodies))})))

(defmethod enhance-code 'try [sym->iden [_ & xs]]
  (let [bodies  (remove (fn [x] (and (seq? x) ('#{finally catch} (first x)))) xs)
        catches (filter (fn [x] (and (seq? x) (= 'catch (first x)))) xs)
        finally (some (fn [x] (when (and (seq? x) (= 'finally (first x))) x)) xs)
        catch-identity (gensym)
        bodies  (for [body bodies] (enhance-code sym->iden body))
        catches (for [[c t e & tail] catches
                      :let [sym->iden (assoc sym->iden e catch-identity)]]
                  (list* c t (with-meta e {::symbol-identity catch-identity})
                         (map (partial enhance-code sym->iden) tail)))
        catch-metas    (for [[c t e & tail] catches, x tail] (meta x))
        finally-bodies (seq (for [body (next finally)] (enhance-code sym->iden body)))]
    (with-meta
      (concat '[try]
               bodies
               catches
               (when finally-bodies [(list* 'finally finally-bodies)]))
      {::symbol-used (-> #{}
                         (into (mapcat (comp ::symbol-used meta) bodies))
                         (into (mapcat ::symbol-used catch-metas))
                         (into (mapcat (comp ::symbol-used meta) finally-bodies))
                         (->> (remove #{catch-identity}))
                         (set))
       ::symbol-introduced (-> #{}
                               (into (mapcat (comp ::symbol-introduced meta) bodies))
                               (into (mapcat ::symbol-introduced catch-metas))
                               (into (mapcat (comp ::symbol-introduced meta) finally-bodies))
                               (cond-> (seq catches) (conj catch-identity)))})))

(defmethod enhance-code 'letfn* [sym->iden [letfn* bindings & bodies]]
  (let [sym->iden     (merge sym->iden (zipmap (take-nth 2 bindings) (repeatedly gensym)))
        binding-pairs (for [[var fndef] (partition 2 bindings)]
                        [(with-meta var {::symbol-identity (sym->iden var)})
                         (enhance-code sym->iden fndef)])
        bodies        (for [body bodies] (enhance-code sym->iden body))
        symbol-added  (set (map (comp ::symbol-identity meta first) binding-pairs))
        symbol-used   (-> #{}
                          (into (mapcat (comp ::symbol-used meta second) binding-pairs))
                          (into (mapcat (comp ::symbol-used meta) bodies))
                          (->> (remove symbol-added))
                          (set))
        symbol-introduced (-> #{}
                              (into (mapcat (comp ::symbol-introduced meta) bodies))
                              (into symbol-added))]
    (with-meta
      (list* letfn* (vec (apply concat binding-pairs)) bodies)
      {::symbol-used symbol-used
       ::symbol-introduced symbol-introduced})))

(defn evaluator [expr]
  (let [expr     `((fn* [] ~expr))
        expanded (macroexpand-all-code expr)
        enhanced (enhance-code {} expanded)
        node     (->eval-node {} nil enhanced)]
    ;; array is empty yet.
    (evalme node nil)))

(defn- all-test-namespaces []
  (for [ns (all-ns)
        :when (or (ns-resolve ns 'test-ns-hook)
                  (some (comp :test  meta) (vals (ns-interns ns))))]
    ns))

(defn -main [& args]
  (evaluator '(in-ns 'user))
  (evaluator '(run! require uclj.core/namespaces-to-require))
  (cond
    (and (first args) (.startsWith (str (first args)) "("))
    (println (evaluator (read-string (first args))))

    (and (first args) (.exists (io/file (first args))))
    (do (evaluator `(load-file ~(first args)))
        (when (=  "--test" (second args))
          (let [test-result (apply clojure.test/run-tests (all-test-namespaces))]
            (when-not (zero? (:fail test-result))
              (System/exit 1)))))

    :else ;; interactive mode
    (do (println "Welcome to the small interpreter!")
        (doseq [v [#'*1 #'*2 #'*3 #'*e]] (var-set-reset! v nil))
        (loop []
          (print (str (ns-name *ns*) "=> ")) (flush)
          (let [read (read {:eof ::eof} *in*)]
            (when-not (= ::eof read)
              (try (let [e (evaluator read)]
                     (var-set-reset! #'*3 *2)
                     (var-set-reset! #'*2 *1)
                     (var-set-reset! #'*1 e)
                     (println e))
                   (catch Throwable t
                     (.printStackTrace t)
                     (var-set-reset! #'*e t)))
              (recur))))
        (println "EOF, bye!"))))

:OK
