(ns uclj.core-test
  (:require [clojure.test :refer :all]
            [uclj.core :refer :all]))

(deftest test-let-form
  (is (= :a (evaluator '(let [] :b :a))))
  (is (= nil (evaluator '(let [a 1]))))
  (is (= 1 (evaluator '(let [a 1 b a] b))))
  (is (= 4 (evaluator '(let [a 2 b 3] (let [a 1] (+ a b))))))

  (is (= "ax" (evaluator '(((let [a "a"] (fn fff ([] fff) ([x] (str a x))))) "x"))))

  (testing "Binding works across collections"
    (is (= 4 (evaluator '(first (let [a 4] #{a})))))
    (is (= 4 (evaluator '(ffirst (let [a 4] {a :v})))))
    (is (= 4 (evaluator '(first (let [a 4] [a]))))))

  (testing "asdf"
    (is (fn? (evaluator '(let [b :lol] (fn [] (let [_ b] :hi)))))))

  (testing "Shadow lexical bindings"
    (is (= 4 (evaluator '(let [inc dec] (inc 5)))))))

(deftest test-eval-try
  (testing "try-catch maintains sybol usage across closure"
      (is (= 2 (evaluator '(let [a 1 b 2 c 3]
                              ((fn [] (try (inc a) (catch RuntimeException e (inc b)) (finally (inc c))))))))))

  (is (= 2 (evaluator '(try 1 2 (finally 3)))))
  (is (= [0 0 2] (evaluator '(let [a (atom 0)] [@a (try @a (finally (reset! a 2))) @a]))))
  (is (= 1 (evaluator '(try 1 (catch Throwable t t)))))
  (is (= 1 (evaluator '(try (assert false) (catch Throwable t 1))))))

(deftest test-eval-fn

  (testing "Function without args is called"
    (is (= :b (evaluator '((fn [] :b))))))

  (testing "Function name is used"
    (is (fn? (evaluator '((fn f [] f))))))

  (testing "Recur with single arg"
    (is (= :b (evaluator '((fn [a] (if (pos? a) (recur (dec a)) :b)) 4)))))

  (testing "Recur without args"
    (is (= 0 (evaluator '(let [a (atom 4)]
                           ((fn [] (if (pos? (swap! a dec)) (recur) @a))))))))

  '
  (testing "Recur in vararg forms"
    (is (= :a (evaluator '((fn [_ & [i]] (if (pos? i) (recur :k (list (dec i))) :a)) :p 39 )))))


  )

(defn unbound-var? [v] (and (var? v) (not (bound? v))))
(defn unbound-value? [v] (instance? clojure.lang.Var$Unbound v))

(deftest test-call-binary
  (is (= () (evaluator '())))
  (is (= 23 (evaluator '(+ 20 3))))

  (testing "used vars in case are correctly encapsulated in closure"
    (is (= 7 (evaluator '(let [a 2] ((fn [t] (inc (+ t a))) 4))))))

  :ok)

(deftest test-def
  (testing "Def form declares var"
    (is (unbound-var? (evaluator '(def a1))))
    (is (thrown? Exception (evaluator '(if a (def a)))))
    (is (unbound-value? (evaluator '(if (def bb) bb))))
    (is (unbound-value? (evaluator '(if false (def cc) cc)))))
  (testing "Defining var is being referenced"
    (testing "Recursively defined list"
      (is (var? (evaluator '(def onetwo (cons 1 (cons 2 (lazy-seq onetwo)))))))
      (is (= [1 2 1] (evaluator '(take 3 onetwo)))))
    (testing "Recursive function"
      (is (var? (evaluator '(defn countdown [n] (if (zero? n) :none (countdown (dec n)))))))
      (is (= :none (evaluator '(countdown 5)))))))

(deftest test-interop
  (testing "constructor invocation"
    (is (= 12 (evaluator '(Integer. "12"))))
    (is (= 12 (evaluator '(new Integer "12")))))
  (testing "field access"
    (is (= Integer/MAX_VALUE (evaluator 'Integer/MAX_VALUE))))
  (testing "static method invocation"
    (is (= 12 (evaluator '(Integer/valueOf "12")))))
  (testing "instance method invocation"
    (is (= "12" (evaluator '(.toString 12))))
    (testing "with and without parentheses"
      (is (= "f" (evaluator '(. "asdf" substring 3))))
      (is (= "f" (evaluator '(. "asdf" (substring 3))))))))

(deftest test-fn-form
  (testing "Functin handle is bound"
    (is (fn? (evaluator '((fn f [a] f) 3)))))

  (testing (= 16 (evaluator '(
                (let [x 3 y 4]
                  (fn []
                    (let [f (fn ([a] (+ a x)) ([a b] (+ a b y))  )]
                      (+ (f 1) (f 2 3)))))))))

  (testing "Returns argument"
    (is (= 1 (evaluator '((fn [a] a) 1))))
    (is (= 1 (evaluator '((fn [a b] a) 1 2))))
    (is (= :a (evaluator '((fn [] :a)))))))

(deftest test-letfn-form
  (is (= 1
         (evaluator
          '(letfn [(collatz [n] (cond (= 1 n) 1 (even? n) (div2 n) (odd? n) (add3 n)))
                   (div2 [x] (collatz (/ x 2)))
                   (add3 [x] (collatz (+ 1 (* 3 x))))]
             (collatz 12)))))

  (testing "functions access context from outside"
    (is (= 1 (evaluator '(let [a 1 b 2] (letfn [(x [] a) (y [] (x))] (x)))))))
  (testing "shadowed lexical binding"
    (is (= 4 (evaluator '(letfn [(inc [x] (dec x))] (inc 5)))))
    (is (= 4 (evaluator '(letfn [(a [x] (inc x)) (inc [x] (dec x))] (a 5)))))))

(deftest test-loop-form
  (testing "Bindings are like with let"
    (is (= 2 (evaluator '(loop [i 1 j (inc i)] j))))))

(deftest test-case
  (is (= :one (evaluator '(case 1 1 :one 2 :two 3 :three 4))))

  (testing "can recur from case"
    (testing "recur in default branch"
      (is (= :one (evaluator '(loop [i 12] (case i 0 :one (recur (dec i))))))))
    (testing "recur from branch"
      (is (= :two (evaluator '(loop [i 4] (case i (1 2 3 4) (recur (dec i)) :two))))))
    (testing "Cannot recur from expression"
      (is (thrown? AssertionError ;; TODO: throw other exception type!
                   (evaluator '(loop [i 2] (case (recur (dec i)) 1 1 2 2 :three)))))))

  (testing "used vars in case are correctly encapsulated in closure"
    (is (= :ok 
          (evaluator '(let [a :ok b :w1 c :w2] ((fn [t] (case t, 1 a, 2 b, c)) 1))))))

  (testing "Identity checking because all cases are keywords"
    (testing "All keys have different hashes"
      (is (= 2 (evaluator '(case :b, :a 1 :b 2 :c 3, :_)))))
    (testing "Colliding hashes"
      (is (= 2 (evaluator '(case :AaBB, :AaAa 1 :AaBB 2, :_))))
      (is (= 1 (evaluator '(case :AaAa, :AaAa 1 :AaBB 2, :_))))
      (is (= :_ (evaluator '(case :BBBB, :AaAa 1 :AaBB 2, :_))))))

  (testing "Check integers"
    (is (= :a (evaluator '(case (inc 11), 12 :a 13 :b 14, :c))))
    (is (= :_ (evaluator '(case 112, 12 :a 13 :b 14 :c, :_)))))

  (testing "Check hashes"
    (is (= 1 (evaluator '(case 'a, a 1 b 2 c 3))))
    (testing "Colliding hashes"
      (is (= 2 (evaluator '(case (name 'AaBB), "AaAa" 1 "AaBB" 2, :_))))
      (is (= 1 (evaluator '(case (name 'AaAa), "AaAa" 1 "AaBB" 2, :_))))
      (is (= :_ (evaluator '(case (name 'BBBB), "AaAa" 1 "AaBB" 2, :_)))))))

(deftest test-var-form
  (is #'clojure.core/inc (evaluator '(var inc))))

(deftest test-macroexpand-all-code
  (is (= '(new java.lang.String "")
         (macroexpand-all-code '(java.lang.String. ""))))
  (is (= '(. Integer valueOf 34)
         (macroexpand-all-code '(Integer/valueOf 34))))
  (is (= '(. "" toString)
         (macroexpand-all-code '(.toString ""))))

  )


(deftest test-core-async
  (is (= 34 (evaluator '(clojure.core.async/<!! (clojure.core.async/go 34)))))


  (testing "Macro has access to content of &env"
    (is (= 42 (clojure.core.async/<!! (evaluator '(let [a 42] (clojure.core.async/go a)))))))

  ;; has macro bindings
  (is (= 3 (evaluator '(let [a (clojure.core.async/go 1)
                             b (clojure.core.async/go 2)]
                         (clojure.core.async/<!!
                          (clojure.core.async/go
                            3 (+ (clojure.core.async/<! a) (clojure.core.async/<! b)))))))))

(defmacro macroenv [& xs]
  (list 'quote (sort (keys &env))))

(deftest test-&env-in-macros
  (is (= '(a f)
          (evaluator '((fn f [a] (uclj.core-test/macroenv 2 3)) 3))))
  #_(is (= '(a)
          (evaluator '(let [a 1] (uclj.core-test/macroenv 2 3)))))
  (is (= '(a b)
          (evaluator '(loop [a 1 b 2] (uclj.core-test/macroenv 4 4)))))
  )

(deftest test-accessiable-namespaces
  (is (= true (evaluator '(clojure.string/blank? ""))))
  (is (= #{1 2 3} (evaluator '(clojure.set/union #{1 2} #{2 3})))))

(deftest test-dynamic-bindings
  (testing "Bound dynamically"
    (is (= false (evaluator '(binding [*print-readably* false] *print-readably*)))))
  (testing "Bound dynamically to var defined here"
    (evaluator '(def ^:dynamic *testvar*))
    (is (= :a (evaluator '(binding [*testvar* :a] *testvar*)))))
  (testing "Marked as dynamic in def form"
    (evaluator '(declare ^:dynamic *testvar2*))
    (is (= :a (evaluator '(binding [*testvar2* :a] *testvar2*)))))
  (testing "bound-fn works"
    (evaluator '(declare ^:dynamic *testvar3*))
    (is (= :a (evaluator '(let [f (binding [*testvar3* :a] (bound-fn [] *testvar3*))]
                            (f)))))))
