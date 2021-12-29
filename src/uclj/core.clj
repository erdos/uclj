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

(run! require
      '[;;
        [clojure.core.async :as async]
        ;;[clojure.core.logic :as logic]
        ;clojure.data
        ;clojure.datafy
        ;clojure.data.csv
        ;clojure.data.xml
        ;clojure.edn
        [clojure.java.io :as io]
        ;[clojure.pprint :as pprint]
        ;clojure.string
        ;clojure.set
        ;[clojure.test :refer [deftest testing is are]]
        ;clojure.walk
        ;[clojure.zip :as zip]
        ])

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
           (let [&env (reduce add-env &env (take-nth (second expanded)))]
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

#_ ;; TODO!!
(defmethod eval-seq 'letfn* [let-bindings [_ bindings & bodies :as form]]
  (assert (map? let-bindings))
  (let [bindings (partition 2 bindings)
        promises (for [_ bindings] (promise))
        let-bindings (into let-bindings
                           (map (fn [[k] p] [k (fn [& t] (apply @p t))])
                                bindings promises))]
    (dorun (map (fn [[k v] p] (deliver p (evaluate let-bindings v))) bindings promises))
    (eval-block let-bindings bodies)))

(defn- eval-sym [a x]
  (assert (map? a) (str "Not map " (pr-str a)))
  (assert (symbol? x) (str "Not symbol " (pr-str x)))
  (if-let [[_ v] (find a x)]
    v
    (if-let [resolved (resolve x)]
      (deref resolved)
      (if-let [parent (some-> x namespace symbol resolve)]
        (if (class? parent)
          (clojure.lang.Reflector/getStaticField ^Class parent (name x))
          (assert false (str "Cannot access " (name x) "in namespace " parent)))
        (assert false (str "Cannot resolve " x))))))

(defprotocol EvalNode
  (evalme [this bindings]))

(extend-protocol EvalNode
  nil                            (evalme [_ _] nil)
  String                         (evalme [t _] t)
  Number                         (evalme [t _] t)
  Boolean                        (evalme [t _] t)
  java.util.regex.Pattern        (evalme [t _] t)
  clojure.lang.Keyword           (evalme [t _] t)
  clojure.lang.Symbol            (evalme [t b] (eval-sym b t))
  clojure.lang.IPersistentVector (evalme [t b] (mapv #(evalme % b) t))
  clojure.lang.IPersistentMap    (evalme [t b] (reduce-kv (fn [m k v] (assoc m (evalme k b) (evalme v b))) {} t))
  clojure.lang.IPersistentSet    (evalme [s b] (into (empty s) (for [x s] (evalme x b)))))

;; TODO: test with interfaces instead of protocols!
(defmacro gen-eval-node [body]
  `(reify EvalNode
     (evalme [_ ~'&b] ~body)))

(declare ->eval-node)

(defmulti seq->eval-node first :default ::default)
(defmethod seq->eval-node ::default seq-eval-call [s]
  (let [[f & args] (map ->eval-node s)
        [a1 a2 a3 a4] args]
    (dorun args)
    (case (count args)
      0 (gen-eval-node (.invoke ^clojure.lang.IFn (evalme f &b)))
      1 (gen-eval-node (.invoke ^clojure.lang.IFn (evalme f &b) (evalme a1 &b)))
      2 (gen-eval-node (.invoke ^clojure.lang.IFn (evalme f &b) (evalme a1 &b) (evalme a2 &b)))
      3 (gen-eval-node (.invoke ^clojure.lang.IFn (evalme f &b) (evalme a1 &b) (evalme a2 &b) (evalme a3 &b)))
      4 (gen-eval-node (.invoke ^clojure.lang.IFn (evalme f &b) (evalme a1 &b) (evalme a2 &b) (evalme a3 &b) (evalme a4 &b)))

      ;; else
      (gen-eval-node (apply (evalme f &b) (for [e args] (evalme e &b)))))))

(defmethod seq->eval-node 'quote seq-eval-quote [[_ quoted]] (gen-eval-node quoted))

(defmethod seq->eval-node 'if seq-eval-if [[_ condition then else]]
  (let [condition (->eval-node condition)
        then      (->eval-node then)
        else      (->eval-node else)]
    (if else ;; shortcut: if else branch is missing then no need to call
      (gen-eval-node (if (evalme condition &b) (evalme then &b) (evalme else &b)))
      (gen-eval-node (if (evalme condition &b) (evalme then &b))))))

(defmethod seq->eval-node 'case* seq-eval-case
  [[_ value shift mask default-value imap switch-type mode skip-check :as form]]
  ;; (case* ~ge ~shift ~mask ~default ~imap ~switch-type :int)
  ;; switch-type: sparse vs compact
  ;; mode: ints hashes identity
  ;; - ints: if all keys are integers between MIN and MAX values
  ;; - hash-identity: if all keys are keywords
  ;; - hash-equiv: objects
  (let [imap (zipmap (keys imap)
                     (for [[h branch] (vals imap)]
                       [h (->eval-node branch)]))
        default-value (->eval-node default-value)]
    (gen-eval-node
     (let [ev (evalme value &b)
           eh (clojure.lang.Util/hash ev)
           th (if (zero? mask) eh (bit-and (bit-shift-right eh shift) mask))]
       (if-let [[h branch] (get imap th)]
         (if (and skip-check (not (skip-check th)))
           (if (= eh h)
             (evalme branch &b)
             (evalme default-value &b))
           (evalme branch &b))
         (evalme default-value &b))))))

(defmethod seq->eval-node 'do seq-eval-do [[_ & bodies]]
  (let [bodies (map ->eval-node bodies)]
    (case (count bodies)
      0 nil
      1 (first bodies)
      2 (let [[b1 b2] bodies]
          (gen-eval-node (do (evalme b1 &b) (evalme b2 &b))))
      3 (let [[b1 b2 b3] bodies]
          (gen-eval-node (do (evalme b1 &b) (evalme b2 &b) (evalme b3 &b))))
      ;; else
      (let [butlast-body (doall (butlast bodies))
            last-body    (last bodies)]
        (gen-eval-node
         (do (doseq [x butlast-body] (evalme x &b))
             (evalme last-body &b)))))))

(defmethod seq->eval-node 'fn* seq-eval-fn [form]
  (let [[fname & bodies] (parsed-fn form)
        rest-def (first (filter (fn [[args]] (some #{'&} args)) bodies))
        rest-def-butlast-args (drop-last 2 (first rest-def))
        rest-def-last-arg     (last (first rest-def))
        rest-def-bodies       (next rest-def)
        rest-node             (->eval-node (list* 'do rest-def-bodies))
        bodies (remove #{rest-def} bodies)
        arity->args      (reduce (fn [m [args]] (assoc m (count args) args))
                                 {} bodies)
        arity->body-node (reduce (fn [m [args & bodies :as def]]
                                   (assoc m (count args) (->eval-node (list* 'do bodies))))
                                 (if rest-def-bodies
                                   {:variadic (->eval-node (list* 'do rest-def-bodies))}
                                   {})
                                 bodies)
        arity->def (reduce (fn [m [args & bodies :as def]]
                             (assoc m (count args) def)) {} bodies)]
    (cond
      (and (not rest-def) (= 1 (count bodies)) (arity->args 0))
      (let [body (arity->body-node 0)]
        (gen-eval-node
         (fn nullary []
           (let [&b (if fname (assoc &b fname nullary) &b)
                 call-result (evalme body &b)]
             (if (instance? Recur call-result)
               (recur)
               call-result)))))

      (and (not rest-def) (= 1 (count bodies)) (arity->args 1))
      (let [a1name (first (arity->args 1))
            body   (arity->body-node 1)]
        (gen-eval-node
         (fn unary [a]
           (let [&b (if fname (assoc &b fname unary) &b)
                 &b (assoc &b a1name a)
                 call-result (evalme body &b)]
             (if (instance? Recur call-result)
               (recur (first (:bindings call-result)))
               call-result)))))

      (and (not rest-def) (= 1 (count bodies)) (arity->args 3))
      (let [[a1name a2name a3name] (arity->args 3)
            body   (arity->body-node 3)
            third (fn [[_ _ x]] x)]
        (gen-eval-node
         (fn ternary [a1 a2 a3]
           (let [&b (if fname (assoc &b fname ternary) &b)
                 &b (assoc &b a1name a1 a2name a2 a3name a3)
                 call-result (evalme body &b)]
             (if (instance? Recur call-result)
               (recur (first (:bindings call-result))
                      (second (:bindings call-result))
                      (third (:bindings call-result)))
               call-result)))))

      (and (not rest-def) (= 1 (count bodies)) (arity->args 4))
      (let [[a1name a2name a3name a4name] (arity->args 4)
            body   (arity->body-node 4)
            third (fn [[_ _ x]] x)
            fourth (fn [[_ _ _ x]] x)]
        (gen-eval-node
         (fn quaternary [a1 a2 a3 a4]
           (let [&b (if fname (assoc &b fname quaternary) &b)
                 &b (assoc &b a1name a1 a2name a2 a3name a3 a4name a4)
                 call-result (evalme body &b)]
             (if (instance? Recur call-result)
               (recur (first (:bindings call-result))
                      (second (:bindings call-result))
                      (third (:bindings call-result))
                      (fourth (:bindings call-result)))
               call-result)))))


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
             call-result)))))))

(defmethod seq->eval-node 'let* seq-eval-let [[_ bindings & bodies]]
  (let [body-node (seq->eval-node (list* 'do bodies))
        let-pairs (for [[k v] (partition 2 bindings)]
                    [k (->eval-node v)])]
    (case (count bindings)
      0 body-node
      2 (let [[[k b]] let-pairs]
          (gen-eval-node (evalme body-node (assoc &b k (evalme b &b)))))

      4 (let [[[k1 b1] [k2 b2]] let-pairs]
          (gen-eval-node
           (let [&b (assoc &b k1 (evalme b1 &b))
                 &b (assoc &b k2 (evalme b2 &b))]
             (evalme body-node &b))))

      6 (let [[[k1 b1] [k2 b2] [k3 b3]] let-pairs]
          (gen-eval-node
           (let [&b (assoc &b k1 (evalme b1 &b))
                 &b (assoc &b k2 (evalme b2 &b))
                 &b (assoc &b k3 (evalme b3 &b))]
             (evalme body-node &b))))


      ;; else TODO:
      (gen-eval-node
       (evalme body-node (reduce (fn [m [k v]]
                                   (if (= k '_)
                                     (do (evalme v m) m)
                                     (assoc m k (evalme v m))))
                                 &b let-pairs))))))

;; TODO: need to build
(defmethod seq->eval-node 'loop* seq-eval-loop [[_ bindings & bodies]]
  (assert (even? (count bindings)))
  (let [bindings (for [[k v] (partition 2 bindings)]
                   [k (->eval-node v)])
        body-node (seq->eval-node (list* 'do bodies))]
    (case (count bindings)
      1
      (let [[[k v]] bindings]
        (gen-eval-node
         (loop [&b (assoc &b k (evalme v &b))]
           (let [last-res (evalme body-node &b)]
             (if (instance? Recur last-res)
               (recur (assoc &b k (first (:bindings last-res))))
               last-res)))))

      ;; else
      (gen-eval-node
       (loop [&b (reduce (fn [&b [k v]]
                           (assoc &b k (evalme v &b)))
                         &b bindings)]
         (let [last-res (evalme body-node &b)]
           (if (instance? Recur last-res)
             (recur (into &b (zipmap (map first bindings) (:bindings last-res))))
             last-res)))))))

(defmethod seq->eval-node 'new seq-eval-new [[_ class-name & args]]
  (let [clz (symbol->class class-name)
        _   (assert clz (str "Unexpected class name: " class-name))
        args (mapv ->eval-node args)]
    (case class-name
      ;; inline direct calls
      clojure.lang.LazySeq (gen-eval-node (new clojure.lang.LazySeq (evalme (first args) &b)))

      ;; else
      (gen-eval-node
       (let [args (for [a args] (evalme a &b))]
         (clojure.lang.Reflector/invokeConstructor clz (into-array Object args)))))))

(defmethod seq->eval-node 'recur seq-eval-recur [[_ & values]]
  (let [nodes (mapv ->eval-node values)
        size  (count nodes)
        cache (object-array size)]
    (gen-eval-node
     (loop [i 0]
       (if (< i size)
         (do (aset cache i (evalme (nth nodes i) &b))
             (recur (inc i)))
         (->Recur cache))))))

(defmethod seq->eval-node 'throw [[_ e :as form]]
  (assert (= 2 (count form)))
  (assert (or (symbol? e) (seq? e)))
  (let [e (->eval-node e)]
    (gen-eval-node (throw (evalme e &b)))))

(defmethod seq->eval-node 'clojure.core/eval [[_ e]]
  (let [e (->eval-node e)]
    (gen-eval-node (-> e (evalme &b) (evalme nil)))))

(defmethod seq->eval-node 'var [[_ x]]
  (let [x (resolve x)] ;; TODO!
    (assert x (str "Unable to resolve var: " x "in this context"))
    (gen-eval-node x)))

(defmethod seq->eval-node '. [[_ target field & args]]

  ;; TODO: how is it possible that symbol is already resolved to Class here?
  (let [[field args] (if (seq? field) [(first field) (next field)] [field args])]
    (let [args (map ->eval-node args)]
      (if-let [target-class (cond (class? target) target
                                  (and (symbol? target) (class? (resolve target))) (resolve target))]
        (gen-eval-node
         (clojure.lang.Reflector/invokeStaticMethod
          ^Class target-class
          (name field)
          ^objects (into-array Object (for [a args] (evalme a &b)))))
        (let [target (->eval-node target)]
          (if (= 'nth field) ;; very common in let* forms
            (gen-eval-node
             (nth (evalme target &b)
                  (evalme (first args) &b)
                  (evalme (second args) &b)))
            (gen-eval-node
             (clojure.lang.Reflector/invokeInstanceMethod
              (evalme target &b) (name field) (into-array Object (for [a args] (evalme a &b)))))))))))

(defmethod seq->eval-node 'try [[_ & xs]]
  (let [catch-clauses  (keep (fn [x] (when (and (seq? x) (= 'catch (first x)))
                                       [(resolve (nth x 1)) ;; type
                                        (nth x 2) ;; variable
                                        (->eval-node (list* 'do (nthrest x 2)))]))
                             xs)
        bodies         (remove (fn [x] (and (seq? x) ('#{finally catch} (first x)))) xs)
        body-node (seq->eval-node (list* 'do bodies))
        finally-node (some (fn [x] (when (and (seq? x) (= 'finally (first x)))
                                     (->eval-node (list* 'do (next x))))) xs)]
    (gen-eval-node
     (try (evalme body-node &b)
          (catch Throwable t
            (if-let [[_ v node] (some (fn [[c v node :as item]] (when (instance? c t) item)) catch-clauses)]
              (evalme node (assoc &b v t))
              (throw t)))
          (finally (evalme finally-node &b))))))

(defn ->eval-node [expr]
  (cond (seq? expr)  (seq->eval-node expr)
        (map? expr)  (persistent! (reduce-kv (fn [a k v] (assoc! a (->eval-node k) (->eval-node v))) (empty expr) (transient expr)))
        (coll? expr) (into (empty expr) (map ->eval-node expr))
        :else expr))



(defn expand-and-eval [expr]
  (let [expanded (macroexpand-all-code expr)
        node     (->eval-node expanded)]
    ; (println :> expanded)
    (evalme node basic-bindings)))

(def evaluator expand-and-eval)

(defn -main [& args]
  (cond
    (and (first args) (.startsWith (str (first args)) "("))
    (println (expand-and-eval (read-string (first args))))

    (and (first args) (.exists (io/file (first args))))
    (with-open [in (new java.io.PushbackReader (io/reader (io/file (first args))))]
      (loop []
        (let [read (read {:eof ::eof} in)]
          (when-not (= ::eof read)
            (expand-and-eval read)
            (recur)))))

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
