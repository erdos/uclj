(ns uclj.core
  (:gen-class)
  (:import [clojure.lang Gateway Var LazySeq]
           [java.lang Number]
           [java.io StringWriter]
           [java.util.concurrent.atomic AtomicReferenceArray]))

(set! *warn-on-reflection* true)
#_
(def basic-bindings
  {'clojure.lang.Var clojure.lang.Var
   'clojure.lang.LazySeq clojure.lang.LazySeq
   'java.util.concurrent.atomic.AtomicReferenceArray java.util.concurrent.atomic.AtomicReferenceArray})

(def namespaces-to-require
  '[[clojure.core]
    [clojure.core.async :as async]
                                        ;[clojure.core.logic :as logic]
                                        ;clojure.data
                                        ;clojure.datafy
                                        ;clojure.data.csv
                                        ;clojure.data.xml
                                        ;clojure.edn
    [clojure.java.io :as io]
                                        ;[clojure.pprint :as pprint]
    clojure.string
    clojure.set
                                        ;[clojure.test :refer [deftest testing is are]]
                                        ;clojure.walk
                                        ;[clojure.zip :as zip]
    ])

(run! require namespaces-to-require)

(defn- array? [x] (when x (.isArray (class x))))

(defmacro template [expr]
  (letfn [(unroll [expr] (cond (seq? expr) (unroll-seq expr)
                               (vector? expr) [(vec (mapcat unroll expr))]
                               :else [expr]))
          (unroll-seq [[t s :as expr]]
            (cond ('#{clojure.core/unquote-splicing} t) (eval s)
                  ('#{clojure.core/unquote} t)          [(eval s)]
                  :else                                 [(mapcat unroll expr)]))]
    (first (unroll expr))))

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
  (let [e (macroexpand-1-code &env exp)] (if (identical? e exp) exp (recur &env e))))

; (macroexpand-code {} '(fn [a [b c] d] (+ b c)))

;; return seq of (fn-name ([args*] bodies*)+)
(defn- parsed-fn [[_ & bodies]]
  (let [fname (when (symbol? (first bodies)) (first bodies))
        bodies (if fname (next bodies) bodies)
        bodies (if (vector? (first bodies)) (list bodies) bodies)]
    (cons fname bodies)))

;; (parsed-fn '(fn x [a [b _] c] 3))
;; (parsed-fn '(fn ([a b[ f g] c] 3)))

;;  TODO
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

         (or (vector? expanded) (set? expanded))
         (into (empty expanded) (map (partial iter &env) expanded))

         (map? expanded)
         (into {} (for [[k v] expanded] [(iter &env k) (iter &env v)]))

         :else ;; scalars
         expanded)))
   {} exp))


(defrecord Recur [bindings])

(defprotocol EvalNode
  (evalme [this ^clojure.lang.IPersistentMap bindings]))

;; TODO: inst and uuid?
(extend-protocol EvalNode
  nil                            (evalme [_ _] nil)
  Number                         (evalme [t _] t)
  Boolean                        (evalme [t _] t)
  Character                      (evalme [t _] t)
  String                         (evalme [t _] t)
  java.util.regex.Pattern        (evalme [t _] t)
  clojure.lang.Keyword           (evalme [t _] t)
  clojure.lang.IPersistentVector (evalme [t b] (mapv #(evalme % b) t))
  clojure.lang.IPersistentMap    (evalme [t b] (reduce-kv (fn [m k v] (assoc m (evalme k b) (evalme v b))) {} t))
  clojure.lang.IPersistentSet    (evalme [s b] (into (empty s) (for [x s] (evalme x b)))))

;; TODO: test with interfaces instead of protocols!
(defmacro gen-eval-node
  ([m body] `(with-meta (gen-eval-node ~body) ~m))
  ([body] `(reify EvalNode (evalme [_ ~'&b] ~body))))

(declare ->eval-node)

;; &a is a hash-map of identity-symbol to index in local bindings.
(defmulti seq->eval-node (fn [iden->idx recur-indices form]
                           (assert (map? iden->idx))
                           (first form))
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
  (let [[f & args] (map (partial ->eval-node &a nil) s)
        [a1 a2 a3 a4 a5 a6 a7] args]
    (dorun args)
    (if-let [call-factory (clojure-core-inlined-fns (::var (meta f)))]
      (apply call-factory args)
      (template
       (case (count args)
         ~@(for [i (range 8)]
             [i (list 'gen-eval-node (list* '.invoke (quote ^clojure.lang.IFn (evalme f &b))
                                             (for [j (range 1 (inc i))] (list 'evalme (symbol (str 'a j)) '&b))))])
         ;; else
         (gen-eval-node (apply (evalme f &b) (for [e args] (evalme e &b)))))))))

(defmethod seq->eval-node 'quote seq-eval-quote [_ _ [_ quoted]] (gen-eval-node quoted))

(defmethod seq->eval-node 'if seq-eval-if [&a recur-indices [_ condition then else]]
  (let [condition (->eval-node &a recur-indices condition)
        then      (->eval-node &a recur-indices then)
        else      (->eval-node &a recur-indices else)]
    (if else ;; shortcut: if else branch is missing then no need to call
      (gen-eval-node (if (evalme condition &b) (evalme then &b) (evalme else &b)))
      (gen-eval-node (if (evalme condition &b) (evalme then &b))))))

(defmethod seq->eval-node 'def seq-eval-def [&a recur-indices [_ def-name & def-bodies]]
  (let [[docstring def-value] (if (= 2 (count def-bodies))
                                def-bodies
                                [nil (first def-bodies)])
        var-object ^clojure.lang.Var (intern *ns* def-name)
        value-node (->eval-node &a recur-indices def-value)]
    (when (:dynamic (meta def-name))
      (.setDynamic var-object))
    (if (not-empty def-bodies)
      (gen-eval-node (doto var-object (.bindRoot (evalme value-node &b))))
      (gen-eval-node var-object))))

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

(defmethod seq->eval-node 'do seq-eval-do [sym->iden recur-indices [_ & bodies]]
  (let [bodies (concat (map  (partial ->eval-node sym->iden nil) (butlast bodies))
                       (list (->eval-node sym->iden recur-indices (last bodies))))
        [b1 b2 b3 b4 b5 b6 b7 b8 b9] bodies]
    (template
     (case (count bodies)
       0 nil
       1 (first bodies)

       ~@(mapcat seq
                 (for [i (range 2 10)]
                   [i (list 'gen-eval-node
                             (list* 'do (for [j (range 1 (inc i))]
                                          (list 'evalme (symbol (str 'b j)) '&b))))]))

       ;; else
       (let [butlast-body (doall (butlast bodies))
             last-body    (last bodies)]
         (gen-eval-node
          (do (doseq [x butlast-body] (evalme x &b))
              (evalme last-body &b))))))))


#_ ;; TODO!
(defmethod seq->eval-node 'letfn* seq-eval-letfn [&a recur-indices [_ bindings & bodies :as form]]
  (let [&a (into &a (for [k (take-nth 2 bindings)] [k ::letfn-binding]))
        bindings (for [[k v] (partition 2 bindings)]
                   [k (seq->eval-node &a v) (promise)])
        letfn-bindings (into {} (for [[k v p] bindings]
                                  [k (fn [& x] (apply @p x))]))
        body-node (->eval-node &a (list* 'do bodies))]
    (gen-eval-node
     (let [&b (merge &b letfn-bindings)]
       (doseq [[k v p] bindings]
         (deliver p (evalme v &b)))
       (evalme body-node &b)))))

(def ^:private kvs-seq (repeatedly #(vector (gensym "k") (gensym "v"))))

(defmethod seq->eval-node 'fn* seq-eval-fn [&a recur-indices form]
  (assert (meta form))
  (let [[fname & bodies]      (parsed-fn form)
        &a                    (if fname (assoc &a fname ::fn-name-binding) &a)
        rest-def              (first (filter (fn [[args]] (some #{'&} args)) bodies))
        rest-def-butlast-args (drop-last 2 (first rest-def))
        rest-def-last-arg     (last (first rest-def))
        rest-def-bodies       (next rest-def)
        rest-node             (when rest-def
                                (->eval-node (into &a (for [k (cons rest-def-last-arg rest-def-butlast-args)]
                                                        [k ::fn-arg-binding]))
                                             recur-indices
                                             (list* 'do rest-def-bodies)))
        bodies (remove #{rest-def} bodies)
        arity->args      (reduce (fn [m [args]] (assoc m (count args) args))
                                 {} bodies)
        arity->body-node  (into (if rest-def-bodies
                                  ;; TODO!
                                  {:variadic (->eval-node &a recur-indices (list* 'do rest-def-bodies))}
                                  {})
                                (for [[args & bodies :as def] bodies
                                      :let [&a (zipmap (concat (::symbol-used (meta def))
                                                               (::symbol-introduced (meta def)))
                                                       (range))
                                            recur-indices (mapv &a (::symbol-loop (meta def)))]]
                                  [(count args) (->eval-node &a recur-indices (list* 'do bodies))]))
        arity->def (reduce (fn [m [args & bodies :as def]]
                             (assoc m (count args) def)) {} bodies)]
    (let [symbol-used    (::symbol-used (meta form))
          enclosed-count (count symbol-used)
          symbol->idx    {}
          [body0 body1 body2] (map arity->body-node (range))
          [body0-symbols body1-symbols body2-symbols] (map (comp ::symbol-introduced meta arity->def) (range))]
      (assert (set? symbol-used))
      ; (println :HELLO body0-symbols body1-symbols)
      (gen-eval-node
       (let [enclosed-array (object-array enclosed-count)]
         ; (assert (array? &b) (str "No array: " (pr-str &b)))
         (doseq [[idx sym] (map vector (range) symbol-used)]
           (aset enclosed-array idx (aget #^objects &b (symbol->idx sym))))
         (fn
           ([]
            (let [invocation-array (java.util.Arrays/copyOf enclosed-array (+ (count body0-symbols) enclosed-count))]
              (loop []
                (let [result (evalme body0 invocation-array)]
                  (if (identical? ::recur result)
                    (recur)
                    result)))))
           ([x]
            (let [invocation-array (java.util.Arrays/copyOf enclosed-array (+ (count body1-symbols) enclosed-count))]
              ;; also: fill f with arguments
              (aset invocation-array (+ 0 enclosed-count) x)
              (loop []
                (let [result (evalme body1 invocation-array)]
                  (if (identical? ::recur result)
                    (recur)
                    result)))))
           ([x y]
            (let [invocation-array (java.util.Arrays/copyOf enclosed-array (+ (count body2-symbols) enclosed-count))]
              ;; also: fill f with arguments
              (aset invocation-array (+ 0 enclosed-count) x)
              (aset invocation-array (+ 1 enclosed-count) y)
              (loop []
                (let [result (evalme body2 invocation-array)]
                  (if (identical? ::recur result)
                    (recur)
                    result)))))
           ;; TODO: generate for all arities
           ))))))

(defmethod seq->eval-node 'let* seq-eval-let [iden->idx recur-indices [_ bindings & bodies]]
  ; (println :seq->eval-node :let* iden->idx bindings (map meta bindings) (map meta bodies))
  (cond
    ;; can merge (let*) forms
    (and (= 1 (count bodies)) (seq? (first bodies)) (= 'let* (ffirst bodies)))
    (recur iden->idx recur-indices (list* 'let*
                                           (into bindings (second (first bodies)))
                                           (nnext (first bodies))))

    :else
    (let [[[idx1 node1]
           [idx2 node2]
           [idx3 node3]] (for [[k v] (partition 2 bindings)]
                           [(int (iden->idx (::symbol-identity (meta k))))
                            (->eval-node iden->idx nil v)])
          body-node (seq->eval-node iden->idx recur-indices (list* 'do bodies))]
      (case (count bindings)
        0 body-node

        2 (gen-eval-node
           (do (aset #^objects &b idx1 (evalme node1 &b))
               (evalme body-node &b)))
        4 (gen-eval-node
           (do (aset #^objects &b idx1 (evalme node1 &b))
               (aset #^objects &b idx2 (evalme node2 &b))
               (evalme body-node &b)))
        6 (gen-eval-node
           (do (aset #^objects &b idx1 (evalme node1 &b))
               (aset #^objects &b idx2 (evalme node2 &b))
               (aset #^objects &b idx3 (evalme node3 &b))
               (evalme body-node &b)))))))

(defmethod seq->eval-node 'loop* seq-eval-loop [iden->idx _ [_ bindings & bodies :as def]]
  (assert (even? (count bindings)))
  (let [[arg1-node arg2-node arg3-node] (for [[k v] (partition 2 bindings)] (->eval-node iden->idx nil v))
        recur-indices                   (mapv iden->idx (::symbol-loop (meta def)))
        body-node                       (seq->eval-node iden->idx recur-indices (list* 'do bodies))
        [idx1 idx2 idx3]                recur-indices]
    (case (count bindings)
      4 (gen-eval-node
         (do (aset #^objects &b idx1 (evalme arg1-node &b))
             (aset #^objects &b idx2 (evalme arg2-node &b))
             (loop []
               (let [result (evalme body-node &b)]
                 (if (identical? ::recur result)
                   (recur)
                   result)))))

      2 (gen-eval-node
         (do (aset #^objects &b idx1 (evalme arg1-node &b))
             (loop []
               (let [result (evalme body-node &b)]
                 (if (identical? ::recur result)
                   (recur)
                   result)))))
      0 (gen-eval-node
         (do (loop []
               (let [result (evalme body-node &b)]
                 (if (identical? ::recur result)
                   (recur)
                   result))))))))

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

(defmethod seq->eval-node 'recur seq-eval-recur [iden->idx recur-indices [_ & values]]
  (assert recur-indices "Recur is not in tail position!")
  (assert (= (count recur-indices) (count values)) "Recur argument count mismatch!")
  (let [nodes (map (partial ->eval-node iden->idx nil) values)
        [n1 n2 n3 n4] nodes
        [i1 i2 i3 i4] recur-indices] ;; TODO!!
    (case (count nodes)
      0 (gen-eval-node (let [] ::recur))
      1 (gen-eval-node (let [v1 (evalme n1 &b)]
                         (aset #^objects &b i1 v1)
                         ::recur))
      2 (gen-eval-node (let [v1 (evalme n1 &b)
                             v2 (evalme n2 &b)]
                         (aset #^objects &b i1 v1)
                         (aset #^objects &b i2 v2)
                         ::recur)))
    ;; TODO: more arities with template!
    ))

(defmethod seq->eval-node 'throw [&a _ [_ e :as form]]
  (assert (= 2 (count form)))
  (assert (or (symbol? e) (seq? e)))
  (let [e (->eval-node &a nil e)]
    (gen-eval-node (throw (evalme e &b)))))

(defmethod seq->eval-node 'clojure.core/eval [&a _ [_ e]]
  (let [e (->eval-node &a nil e)]
    (gen-eval-node (-> e (evalme &b) (evalme nil)))))

;; TODO: is it correct?
(defmethod seq->eval-node 'var [&a _ [_ v]]
  (let [x (resolve v)] ;; TODO!
    (assert x (str "Unable to resolve var: " (pr-str v) " in this context in ns " *ns*))
    (gen-eval-node x)))

(defmethod seq->eval-node '. [&a _ [_ target field & args]]

  ;; TODO: how is it possible that symbol is already resolved to Class here?
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

(declare expand-and-eval)

(doseq [lf '[in-ns clojure.core/in-ns]]
  (defmethod seq->eval-node lf [&a _ [_ nssym]]
    (let [sym-node (->eval-node &a nil nssym)]
      (gen-eval-node
       (let [new-ns (create-ns (evalme sym-node &b))]
         ; (println :>! new-ns)
         (alter-meta! (var *ns*) assoc :dynamic true)
         (try
           ;; works in Clojure REPL but does not compile with graal!
           (.doSet (var *ns*) new-ns)
           (catch IllegalStateException _
             ;; compiles but does nothing in REPL
             (.doReset (var *ns*) new-ns))))))))

(doseq [lf '[clojure.core/load-file load-file]]
  (defmethod seq->eval-node lf [&a _ [_ fname]]
    (let [arg (->eval-node &a nil fname)]
      (gen-eval-node
       (with-open [in (new java.io.PushbackReader (io/reader (io/file (evalme arg &b))))]
         (doseq [read (repeatedly #(read {:eof ::eof} in))
                 :while (not= ::eof read)]
           (expand-and-eval read)))))))

(defmethod seq->eval-node 'clojure.core/with-loading-context [&a _ [_ & bodies]]
  ;; needed by (ns) forms
  (seq->eval-node &a nil (list* 'do bodies)))

;; TODO!
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

(defn ->eval-node [sym->iden recur-indices expr]
  (when recur-indices
    (every? integer? recur-indices))
  (cond (seq? expr)  (seq->eval-node sym->iden recur-indices expr)
        (map? expr)  (persistent! (reduce-kv (fn [a k v] (assoc! a (->eval-node sym->iden nil k) (->eval-node sym->iden nil v))) (transient (empty expr))  expr))
        (coll? expr) (into (empty expr) (map (partial ->eval-node sym->iden nil) expr))
        (symbol? expr) (sym->eval-node sym->iden expr)
        :else expr))

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

(defmethod enhance-code clojure.lang.Symbol [sym->iden s]
  (if-let [iden (get sym->iden s)]
    (with-meta s {::symbol-identity iden, ::symbol-used #{iden}})
    s))

;; TODO
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
                                           (concat bodies (partition 1 2 (next bindings))))))
          symbol-introduced (->> introduced-idents
                                 (into (mapcat (comp ::symbol-introduced meta) bodies))
                                 (into (mapcat (comp ::symbol-introduced meta) bindings)))]
      (with-meta (list* form bindings bodies)
        {::symbol-used       symbol-used
         ::symbol-introduced symbol-introduced
         ::symbol-loop       (when (= form 'loop*) introduced-idents)}))))

(defmethod enhance-code 'fn* [sym->iden fn-expression]
  (let [[fname & fbodies] (parsed-fn fn-expression)
        fbodies (for [[args & bodies] fbodies
                      :let [new-acc   (cond-> (zipmap (remove #{'&} args) (repeatedly gensym))
                                        fname (assoc fname (gensym)))
                            new-acc-1 (zipmap (vals new-acc) (keys new-acc))
                            sym->iden (merge sym->iden new-acc)
                            bodies    (for [body bodies] (enhance-code sym->iden body))
                            args      (mapv (fn [s] (with-meta s {::symbol-identity (new-acc s)})) args)]]
                  (with-meta (list* args bodies)
                    {::symbol-used       (set (remove new-acc-1 (mapcat (comp ::symbol-used meta) bodies)))
                     ::symbol-introduced (into (set (keys new-acc-1))
                                               ;; TODO: also add fn name when needed!!!
                                               (mapcat (comp ::symbol-introduced meta) bodies))
                     ::symbol-loop       (set (keys new-acc-1))}))]
    (with-meta (if fname (list* 'fn* fname fbodies) (list* 'fn* fbodies))
      ;; symbol-introduced is nil because it is a new closure!
      {::symbol-used (set (mapcat (comp ::symbol-used meta) fbodies))})))

(defmethod enhance-code 'try [sym->iden [_ & xs]]
  (let [bodies  (remove (fn [x] (and (seq? x) ('#{finally catch} (first x)))) xs)
        catches (filter (fn [x] (and (seq? x) (= 'catch (first x)))) xs)
        finally (some (fn [x] (when (and (seq? x) (= 'finally (first x))) x)) xs)
        ,,,,,
        catch-identity (gensym)
        bodies  (for [body bodies] (enhance-code sym->iden body))
        catches (for [[c t e & tail] catches
                      :let [sym->iden (assoc sym->iden e catch-identity)]]
                  (list* c t (with-meta e {::symbol-identity catch-identity})
                         (for [body tail] (enhance-code sym->iden body))))
        catch-metas    (for [[c t e & tail] catches, x tail] (meta x))
        finally-bodies (seq (for [body (next finally)] (enhance-code sym->iden body)))]
    (with-meta
      (concat '[try]
               bodies
               catches
               (when finally-bodies [(list* 'finally finally-bodies)]))
      {::symbol-used (-> #{}
                         (into (mapcat (comp ::symbol-used meta) bodies))
                         (into (mapcat ::symbol-introduced catch-metas))
                         (into (mapcat (comp ::symbol-used meta) finally-bodies)))
       ::symbol-introduced (-> #{}
                               (into (mapcat (comp ::symbol-introduced meta) bodies))
                               (into (mapcat ::symbol-introduced catch-metas))
                               (into (mapcat (comp ::symbol-introduced meta) finally-bodies))
                               (cond-> (seq catches) (conj catch-identity)))})))

(defmethod enhance-code 'letfn* [sym->iden [_ bindings bodies]]
  ;; TODO: add acc
  nil)

;; set and vector: recursively run for all elements and merge meta keys
(doseq [t [clojure.lang.IPersistentVector clojure.lang.IPersistentSet]]
  (defmethod enhance-code t [sym->iden v]
    (let [elems (for [b v] (enhance-code sym->iden b))]
      (with-meta (into (empty v) elems)
        {::symbol-used       (set (mapcat (comp ::symbol-used meta) elems))
         ::symbol-introduced (set (mapcat (comp ::symbol-introduced meta) elems))}))))

(defn expand-and-eval [expr]
  (let [expr     `((fn* [] ~expr))
        expanded (macroexpand-all-code expr)
        enhanced (enhance-code {} expanded)
        node     (->eval-node {} nil enhanced)]
    ;; array is empty yet.
    (evalme node nil)))

(def evaluator expand-and-eval)

(defn -main [& args]
  (cond
    (and (first args) (.startsWith (str (first args)) "("))
    (println (expand-and-eval (read-string (first args))))

    (and (first args) (.exists (io/file (first args))))
    (expand-and-eval `(load-file ~(first args)))

    :else ;; interactive mode
    (do (println "Welcome to the small interpreter!")
        (loop []
          (print "$ ") (flush)
          (let [read (read {:eof ::eof} *in*)]
            (when-not (= ::eof read)
              (try (println (expand-and-eval read))
                   (catch Throwable t (.printStackTrace t)))
              (recur))))
        (println "EOF, bye!"))))

(comment

  ;; to test core async:
  (let [a (clojure.core.async/go 1)] (clojure.core.async/<!! (clojure.core.async/go (+ 1 (clojure.core.async/<! a)))))

  )
