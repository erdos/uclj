(ns uclj.core
  (:gen-class)
  (:import [clojure.lang Gateway Var]
           [java.lang Number]
           [java.util.concurrent.atomic AtomicReferenceArray]))

(def basic-bindings
  {'clojure.lang.Var clojure.lang.Var
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

(set! *warn-on-reflection* true)

(declare evaluate)

(defmulti eval-seq (fn [let-bindings form] (first form)) :default ::default)

(defn- special-form? [s] (contains? (methods eval-seq) s))

(defn- add-env [&env sym]
  ;; to ensure content of &env will be symbol->LocalBinding mapping
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
             (class? (ns-resolve *ns* (symbol (namespace (first exp))))))
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

;; return seq of (fn-name ([args*] bodies*)+)
(defn- parsed-fn [[_ & bodies]]
  (let [fname (when (symbol? (first bodies)) (first bodies))
        bodies (if fname (next bodies) bodies)
        bodies (if (vector? (first bodies)) (list bodies) bodies)]
    (cons fname bodies)))


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
           (let [[fname & bodies] (parsed-fn exp)
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

(defmethod eval-seq ::default [a [f & args]]
  (let [e (partial evaluate a)]
    (apply (e f) (map e args))))
(letfn [(a [x] (inc x))] (a 23))

(defn- eval-block [let-bindings bodies]
  (doseq [b (butlast bodies)]
    (evaluate let-bindings b))
  (evaluate let-bindings (last bodies)))

(defmethod eval-seq 'quote [_ [_ q]] q)

(defmethod eval-seq 'do [a [_ & bodies]] (eval-block a bodies))

(defmethod eval-seq 'if [a [_ condition then else]]
  (if (evaluate a condition)
    (evaluate a then)
    (evaluate a else)))

(defrecord Recur [bindings])
(defmethod eval-seq 'recur [a [_ & values]]
  (->Recur (doall (map (partial evaluate a) values))))

(defmethod eval-seq 'loop* [let-bindings [_ bindings & bodies]]
  (assert (even? (count bindings)))
  (let [bindings     (partition 2 bindings)]
    (loop [let-bindings (reduce (fn [let-bindings [k v]]
                                  (assoc let-bindings k (evaluate let-bindings v)))
                                let-bindings bindings)]
      (doseq [v (butlast bodies)]
        (evaluate let-bindings v))
      (let [last-res (evaluate let-bindings (last bodies))]
        (if (instance? Recur last-res)
          (recur (into let-bindings (zipmap (map first bindings) (:bindings last-res))))
          last-res)))))

(defmethod eval-seq 'let* [let-bindings [_ bindings & bodies]]
  (let [let-bindings (reduce (fn [let-bindings [k v]]
                               (assoc let-bindings k (evaluate let-bindings v)))
                             let-bindings (partition 2 bindings))]
    (eval-block let-bindings bodies)))

(defmethod eval-seq 'letfn* [let-bindings [_ bindings & bodies :as form]]
  (assert (map? let-bindings))
  (let [bindings (partition 2 bindings)
        promises (for [_ bindings] (promise))
        let-bindings (into let-bindings
                           (map (fn [[k] p] [k (fn [& t] (apply @p t))])
                                bindings promises))]
    (dorun (map (fn [[k v] p] (deliver p (evaluate let-bindings v))) bindings promises))
    (eval-block let-bindings bodies)))

(defmethod eval-seq 'fn* [a form]
  (let [[fname & bodies] (parsed-fn form)
        rest-def (first (filter (fn [[args]] (some #{'&} args)) bodies))
        rest-def-butlast-args (drop-last 2 (first rest-def))
        rest-def-last-arg     (last (first rest-def))
        rest-def-bodies       (next rest-def)
        bodies (remove #{rest-def} bodies)
        arity->def (reduce (fn [m [args & bodies :as def]]
                             (assoc m (count args) def)) {} bodies)]
    ;; this can be certainly optimized!
    (fn f [& call-args]
      (let [a (if fname (assoc a fname f) a)
            call-result
            (let [c (count call-args)]
              (if-let [[args & bodies] (arity->def c)]
                (let [a (into a (map vector args call-args))]
                  (eval-block a bodies))
                (if rest-def
                  (let [a (-> a
                              (into (zipmap rest-def-butlast-args call-args))
                              (assoc rest-def-last-arg (drop (count rest-def-butlast-args) call-args)))]
                    (eval-block a rest-def-bodies))
                  (assert false "Unexpected arity!"))))]
        (if (instance? Recur call-result)
          (recur (:bindings call-result))
          call-result)))))


(defmethod eval-seq 'case* [a [_ value shift mask default-value imap switch-type mode skip-check :as form]]
  ;; (case* ~ge ~shift ~mask ~default ~imap ~switch-type :int)
  ;; switch-type: sparse vs compact
  ;; mode: ints hashes identity
  ;; - ints: if all keys are integers between MIN and MAX values
  ;; - hash-identity: if all keys are keywords
  ;; - hash-equiv: objects
                                        ;(println :case* form)
  (let [ev (evaluate a value)
        eh (clojure.lang.Util/hash ev)
        th (if (zero? mask) eh (bit-and (bit-shift-right eh shift) mask))
        [h branch] (get imap th)]
    ; (println "value: " ev eh th)
    (if-let [[h branch] (imap th)]
      (if (and skip-check (not (skip-check th)))
        (if (= eh h)
          (evaluate a branch)
          (evaluate a default-value))
        (evaluate a branch))
      (evaluate a default-value))))

(defmethod eval-seq 'throw [a [_ x]]
  (throw (evaluate a x)))

(defmethod eval-seq 'new [a [_ class-name & args]]
  (let [clz (ns-resolve *ns* class-name)
        args (map (partial evaluate a) args)]
    (assert clz (str "Unexpected class name: " class-name))
    ;; TODO: we need to let Graal be aware of most common host sigs
    (clojure.lang.Reflector/invokeConstructor clz (into-array Object args))))

(defmethod eval-seq '. [a [_ target field & args]]
  ;; TODO: how is it possible that symbol is already resolved to Class here?
  (let [[field args] (if (seq? field) [(first field) (next field)] [field args])]
    (if-let [target-class (cond (class? target) target
                                (and (symbol? target) (class? (resolve target))) (resolve target))]
      (clojure.lang.Reflector/invokeStaticMethod
       ^Class target-class
       (name field)
       ^objects (into-array Object (map (partial evaluate a) args)))
      (clojure.lang.Reflector/invokeInstanceMethod
       (evaluate a target) (name field) (into-array Object (map (partial evaluate a) args))))))

#_
(defmethod eval-seq 'clojure.core/binding [a [_ bindings & bodies]]
  ;; even though it is a macro, we are not expanding it further
  )

(defmethod eval-seq 'clojure.core/eval [a [_ form]]
  (->> form (evaluate a) (evaluate nil)))

(defmethod eval-seq 'var [_ [_ x]]
  (or (resolve x)
      (assert false (str "Unable to resolve var: " x "in this context"))))

(defmethod eval-seq 'try [a [_ & xs]]
  (let [finally-bodies (some (fn [x] (when (and (seq? x) (= 'finally (first x))) (next x))) xs)
        ;; ((class-name var-name bodies*)*)
        catch-clauses  (keep (fn [x] (when (and (seq? x) (= 'catch (first x)))
                                       (list* (resolve (second x)) (nnext x)))) xs)
        bodies         (remove (fn [x] (and (seq? x) ('#{finally catch} (first x)))) xs)]
    (try (eval-block a bodies)
         (catch Throwable t
           (if-let [[v & bodies]
                    (some (fn [[c & v+bodies]] (when (instance? c t) v+bodies)) catch-clauses)]
             (eval-block (assoc a v t) bodies)
             (throw t)))
         (finally
           (doseq [b finally-bodies]
             (evaluate a b))))))

(defn- eval-sym [a x]
  (if-let [[_ v] (find a x)]
    v
    (if-let [resolved (resolve x)]
      (deref resolved)
      (if-let [parent (some-> x namespace symbol resolve)]
        (if (class? parent)
          (clojure.lang.Reflector/getStaticField ^Class parent (name x))
          (assert false (str "Cannot access " (name x) "in namespace " parent)))
        (assert false (str "Cannot resolve " x))))))

(defn evaluate [a x]
  (cond (seq? x)    (eval-seq a x)
        (symbol? x) (eval-sym a x)
        (vector? x) (mapv (partial evaluate a) x)
        (set? x)    (set (map (partial evaluate a) x))
        (map? x)    (apply hash-map (for [kv x t kv] (evaluate a t)))
        :else       x))

(defn expand-and-eval [expr]
  (evaluate basic-bindings (macroexpand-all-code expr)))

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
