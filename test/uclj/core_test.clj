(ns uclj.core-test
  (:require [clojure.test :refer :all]
            [uclj.core :refer :all]))

(deftest test-let-form
  (is (= :a (evaluator '(let [] :b :a))))
  (is (= nil (evaluator '(let [a 1]))))
  (is (= 1 (evaluator '(let [a 1 b a] b))))
  (is (= 4 (evaluator '(let [a 2 b 3] (let [a 1] (+ a b)))))))

(deftest test-eval-try
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

(deftest test-case
  ;; (is (= :one (evaluator '(case 1 1 :one 2 :two 3 :three 4))))

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
    (is (some? (evaluator '(let [a 42] (clojure.core.async/go a))))))

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
