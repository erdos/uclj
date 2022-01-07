(ns uclj.core
  (:gen-class)
  (:import [clojure.lang Gateway Var LazySeq]
           [java.lang Number]
           [java.io StringWriter]
           [java.util.concurrent.atomic AtomicReferenceArray]))

(set! *warn-on-reflection* true)

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

(defmulti seq->eval-node (fn [&a form] (first form)) :default ::default)

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

(defmethod seq->eval-node ::default seq-eval-call [&a s]
  (let [[f & args] (map (partial ->eval-node &a) s)
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

(defmethod seq->eval-node 'quote seq-eval-quote [&a [_ quoted]] (gen-eval-node quoted))

(defmethod seq->eval-node 'if seq-eval-if [&a [_ condition then else]]
  (let [condition (->eval-node &a condition)
        then      (->eval-node &a then)
        else      (->eval-node &a else)]
    (if else ;; shortcut: if else branch is missing then no need to call
      (gen-eval-node (if (evalme condition &b) (evalme then &b) (evalme else &b)))
      (gen-eval-node (if (evalme condition &b) (evalme then &b))))))

(defmethod seq->eval-node 'def seq-eval-def [&a [_ def-name & def-bodies]]
  (let [[docstring def-value] (if (= 2 (count def-bodies))
                                def-bodies
                                [nil (first def-bodies)])
        var-object ^clojure.lang.Var (intern *ns* def-name)
        value-node (->eval-node &a def-value)]
    (when (:dynamic (meta def-name))
      (.setDynamic var-object))
    (if (not-empty def-bodies)
      (gen-eval-node (doto var-object (.bindRoot (evalme value-node &b))))
      (gen-eval-node var-object))))

(defmethod seq->eval-node 'case* seq-eval-case
  [&a [_ value shift mask default-value imap switch-type mode skip-check :as form]]
  ;; (case* ~ge ~shift ~mask ~default ~imap ~switch-type :int)
  ;; switch-type: sparse vs compact
  ;; mode: ints hashes identity
  ;; - ints: if all keys are integers between MIN and MAX values
  ;; - hash-identity: if all keys are keywords
  ;; - hash-equiv: objects
  (let [imap (zipmap (keys imap)
                     (for [[h branch] (vals imap)]
                       [h (->eval-node &a branch)]))
        default-value (->eval-node &a default-value)
        value-node    (->eval-node &a value)]
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

(defmethod seq->eval-node 'do seq-eval-do [&a [_ & bodies]]
  (let [bodies (map (partial ->eval-node &a) bodies)
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


(defmethod seq->eval-node 'letfn* seq-eval-letfn [&a [_ bindings & bodies :as form]]
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

(defmethod seq->eval-node 'fn* seq-eval-fn [&a form]
  (let [[fname & bodies]      (parsed-fn form)
        &a                    (if fname (assoc &a fname ::fn-name-binding) &a)
        rest-def              (first (filter (fn [[args]] (some #{'&} args)) bodies))
        rest-def-butlast-args (drop-last 2 (first rest-def))
        rest-def-last-arg     (last (first rest-def))
        rest-def-bodies       (next rest-def)
        rest-node             (->eval-node (into &a (for [k (cons rest-def-last-arg rest-def-butlast-args)]
                                                      [k ::fn-arg-binding]))
                                           (list* 'do rest-def-bodies))
        bodies (remove #{rest-def} bodies)
        arity->args      (reduce (fn [m [args]] (assoc m (count args) args))
                                 {} bodies)
        arity->body-node  (into (if rest-def-bodies
                                  {:variadic (->eval-node &a (list* 'do rest-def-bodies))}
                                  {})
                                (for [[args & bodies :as def] bodies
                                      :let [&a (into &a (for [k args] [k ::fn-arg-binding]))]]
                                  [(count args) (->eval-node &a (list* 'do bodies))]))
        arity->def (reduce (fn [m [args & bodies :as def]]
                             (assoc m (count args) def)) {} bodies)]
    (template
     (cond
       ~@(mapcat seq
                 (for [i (range 0 21)
                       :let [fname (symbol (str 'fnarity i))
                             many-vars (map first (take i kvs-seq))
                             many-keys (map second (take i kvs-seq))]]
                   [`(and (not ~'rest-def) (= 1 (count ~'bodies)) (~'arity->args ~i))
                    `(let [[~@many-vars] (~'arity->args ~i)
                           body#   (~'arity->body-node ~i)]
                       (gen-eval-node
                        (fn ~fname [~@many-keys]
                          (let [~'&b (if ~'fname (assoc ~'&b ~'fname ~(symbol (str 'fnarity i))) ~'&b)
                                ~'&b ~(if (zero? i)
                                        '&b
                                          (list* 'assoc '&b (interleave many-vars many-keys)))
                                call-result# (evalme body# ~'&b)]
                            (if (instance? Recur call-result#)
                              (let [[~@many-vars] (:bindings call-result#)]
                                (recur ~@many-vars))
                              call-result#)))))]))

       :else
       (gen-eval-node
        (fn f [& call-args]
          (let [&b (if fname (assoc &b fname f) &b)
                call-result
                (let [c (count call-args)] ;; TODO: can it be infinite?
                  (if-let [[args] (arity->def c)]
                    (let [&b (into &b (map vector args call-args))]
                      (evalme (arity->body-node c) &b))
                    (if rest-def ;; not node, that can be null also
                      (-> &b
                          (into (zipmap rest-def-butlast-args call-args))
                          (assoc rest-def-last-arg (drop (count rest-def-butlast-args) call-args))
                          (->> (evalme rest-def)))
                      (assert false "Called with unexpected arity!"))))]
            (if (instance? Recur call-result)
              (recur (:bindings call-result))
              call-result))))))))

(def ks+bs (for [i (range 16)] [(symbol (str 'k (inc i))) (symbol (str 'b (inc i)))]))

(defmethod seq->eval-node 'let* seq-eval-let [&a [_ bindings & bodies]]
  (cond
    ;; can merge (let*) forms
    (and (= 1 (count bodies)) (seq? (first bodies)) (= 'let* (ffirst bodies)))
    (recur &a (list* 'let*
                      (into bindings (second (first bodies)))
                      (nnext (first bodies))))

    :else
    (let [[&a let-pairs]
          (loop [&a &a
                 let-pairs []
                 [k v & bindings] bindings]
            (cond (nil? k) [&a let-pairs]
                  :else    (recur (assoc &a k ::let-binding)
                                  (conj let-pairs [k (->eval-node &a v)])
                                  bindings)))
          body-node (seq->eval-node &a (list* 'do bodies))]
      (template
       (let [~(vec ks+bs) (doall let-pairs)]
         (case (count bindings)
           0 body-node
           ;; unroll for common arities!
           ~@(mapcat seq (for [i (range 1 (inc (count ks+bs)))]
                           [(* 2 i)
                            `(gen-eval-node
                              (let [~@(interleave
                                       (repeat '&b)
                                       (for [[k b] (take i ks+bs)]
                                         (list 'assoc '&b k (list 'evalme b '&b))))]
                                (evalme ~'body-node ~'&b)))]))
           (gen-eval-node
            (evalme body-node (reduce (fn [m [k v]]
                                        (if (= k '_)
                                          (do (evalme v m) m)
                                          (assoc m k (evalme v m))))
                                      &b let-pairs)))))))))

(defmethod seq->eval-node 'loop* seq-eval-loop [&a [_ bindings & bodies]]
  (assert (even? (count bindings)))
  (let [[&a bindings]
        (loop [&a &a, let-pairs [], [k v & bindings] bindings]
          (cond (nil? k) [&a let-pairs]
                :else    (recur (assoc &a k ::let-binding)
                                (conj let-pairs [k (->eval-node &a v)])
                                bindings)))
        body-node (seq->eval-node &a (list* 'do bodies))]
    (template
     (case (count bindings)
       ~@(mapcat seq
                 (for [i (range 20)]
                   [i
                    `(let [[~@(take i kvs-seq)] ~'bindings]
                       (gen-eval-node
                        (loop [~'&b (as-> ~'&b ~'&b
                                      ~@(for [[k v] (take i kvs-seq)]
                                          (list 'assoc '&b k (list 'evalme v '&b))))]
                          (let [last-res# (evalme ~'body-node ~'&b)]
                            (if (instance? Recur last-res#)
                              (let [[~@(mapv second (take i kvs-seq))] (:bindings last-res#)]
                                (recur ~(if (zero? i) '&b (list* 'assoc '&b, (flatten (take i kvs-seq))))))
                              last-res#)))))]))))))

(defmethod seq->eval-node 'new seq-eval-new [&a [_ class-name & args]]
  (let [clz (symbol->class class-name)
        _   (assert clz (str "Unexpected class name: " class-name))
        [arg0 :as args] (map (partial ->eval-node &a) args)]
    (case class-name
      ;; inline direct calls
      clojure.lang.LazySeq (gen-eval-node (new clojure.lang.LazySeq (evalme arg0 &b)))

      ;; else
      (gen-eval-node
       (let [args (for [a args] (evalme a &b))]
         (clojure.lang.Reflector/invokeConstructor clz (into-array Object args)))))))

(defmethod seq->eval-node 'recur seq-eval-recur [&a [_ & values]]
  (let [nodes (mapv (partial ->eval-node &a) values)
        size  (count nodes)
        cache (object-array size)] ;; TODO: cache is not thread-safe!
    (gen-eval-node
     (loop [i 0]
       (if (< i size)
         (do (aset cache i (evalme (nth nodes i) &b))
             (recur (inc i)))
         (->Recur cache))))))

(defmethod seq->eval-node 'throw [&a [_ e :as form]]
  (assert (= 2 (count form)))
  (assert (or (symbol? e) (seq? e)))
  (let [e (->eval-node &a e)]
    (gen-eval-node (throw (evalme e &b)))))

(defmethod seq->eval-node 'clojure.core/eval [&a [_ e]]
  (let [e (->eval-node &a e)]
    (gen-eval-node (-> e (evalme &b) (evalme nil)))))

;; TODO: is it correct?
(defmethod seq->eval-node 'var [&a [_ v]]
  (let [x (resolve v)] ;; TODO!
    (assert x (str "Unable to resolve var: " (pr-str v) " in this context in ns " *ns*))
    (gen-eval-node x)))

(defmethod seq->eval-node '. [&a [_ target field & args]]

  ;; TODO: how is it possible that symbol is already resolved to Class here?
  (let [[field args] (if (seq? field) [(first field) (next field)] [field args])]
    (let [[arg1 arg2 arg3 :as args] (map (partial ->eval-node &a) args)]
      (if-let [target-class (cond (class? target) target
                                  (and (symbol? target) (class? (resolve target))) (resolve target))]
        ;; static invocation
        (gen-eval-node
         (clojure.lang.Reflector/invokeStaticMethod
          ^Class target-class
          (name field)
          ^objects (into-array Object (for [a args] (evalme a &b)))))

        ;; instance invocation
        (let [target (->eval-node &a target)]
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
  (defmethod seq->eval-node lf [&a [_ nssym]]
    (let [sym-node (->eval-node &a nssym)]
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
  (defmethod seq->eval-node lf [&a [_ fname]]
    (let [arg (->eval-node &a fname)]
      (gen-eval-node
       (with-open [in (new java.io.PushbackReader (io/reader (io/file (evalme arg &b))))]
         (doseq [read (repeatedly #(read {:eof ::eof} in))
                 :while (not= ::eof read)]
           (expand-and-eval read)))))))

(defmethod seq->eval-node 'clojure.core/with-loading-context [&a [_ & bodies]]
  ;; needed by (ns) forms
  (seq->eval-node &a (list* 'do bodies)))

(defmethod seq->eval-node 'try [&a [_ & xs]]
  (let [catch-clauses  (keep (fn [x] (when (and (seq? x) (= 'catch (first x)))
                                       [(resolve (nth x 1)) ;; type
                                        (nth x 2) ;; variable
                                        (->eval-node (assoc &a (nth x 2) ::catch-var-binding) (list* 'do (nthrest x 2)))]))
                             xs)
        bodies         (remove (fn [x] (and (seq? x) ('#{finally catch} (first x)))) xs)
        body-node (seq->eval-node &a (list* 'do bodies))
        finally-node (some (fn [x] (when (and (seq? x) (= 'finally (first x)))
                                     (->eval-node &a (list* 'do (next x))))) xs)]
    (gen-eval-node
     (try (evalme body-node &b)
          (catch Throwable t
            (if-let [[_ v node] (some (fn [[c v node :as item]] (when (instance? c t) item)) catch-clauses)]
              (evalme node (assoc &b v t))
              (throw t)))
          (finally (evalme finally-node &b))))))

(defn sym->eval-node [&a expr]
  (if (contains? &a expr)
    (gen-eval-node (get &b expr))
    (if (var? (resolve expr))
      ;; TODO: kicsit sok a resolve!
      (let [^clojure.lang.Var resolved-expr (resolve expr)]
        (if (and (bound? resolved-expr) (not (.isDynamic resolved-expr)))
          (let [expr-val @resolved-expr] (gen-eval-node {::var resolved-expr} expr-val))
          (gen-eval-node {::unbound-var resolved-expr} @resolved-expr))) ;; var was unbound at compile time so we need to deref in in runtime
      (if-let [parent (some-> expr namespace symbol resolve)]
        (if (class? parent)
          (gen-eval-node (clojure.lang.Reflector/getStaticField ^Class parent (name expr)))
          (throw (ex-info (str "Cannot access symbol! " expr) {:symbol expr})))
        (throw (ex-info (str "Cannot resolve symbol! " expr) {:symbol expr}))))))

(defn ->eval-node [&a expr]
  (cond (seq? expr)  (seq->eval-node &a expr)
        (map? expr)  (persistent! (reduce-kv (fn [a k v] (assoc! a (->eval-node &a k) (->eval-node &a v))) (transient (empty expr))  expr))
        (coll? expr) (into (empty expr) (map (partial ->eval-node &a) expr))

        ;; TODO: statically lookup common core symbols! check that symbol is not yet bound!!!!!
        (symbol? expr) (sym->eval-node &a expr)
        :else expr))

(defn expand-and-eval [expr]
  (let [expanded (macroexpand-all-code expr)
        node     (->eval-node {} expanded)]
    (evalme node basic-bindings)))

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
