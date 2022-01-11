(ns clojure-core-test
  (:require [clojure.test :refer :all]))

(deftest test-numbers
  (clojure.test/is (= 2 (inc 1)))
  (clojure.test/is (= 0 (dec 1))))

(deftest test-def-meta
  (is (= "123"  (with-out-str (print 1) (def ^{:zzzz (print 2)} two) (print 3))))
  (is (= "123"  (with-out-str (print 1) (def ^{:dynamic (print 2)} two) (print 3))))
  (is (= "1234" (with-out-str (print 1) (def ^{:z (print 2)} two (print 3)) (print 4))))
  (is (= "1234" (with-out-str (print 1) (def ^{:dynamic (print 2) :zzzz (print 3)} three) (print 4)))))

(declare x)

(deftest test-var-behaviour
  (testing "Redef var that has been declared"
    (def x 1)
    (is (= 1 x))
    (def x 2)
    (is (= 2 x)))

  #_
  (testing "Redef var that has not yet been declared"
    (def y 1)
    (is (= 1 y))
    (def y 2)
    (is (= 2 y)))


  (testing "var-set and with-local-vars"
    (with-local-vars [x 1]
      (is (= 1 @x))
      (var-set x 2)
      (is (= 2@x)))))

(deftest test-expr-meta
  (testing "Meta is evaluated for def forms"
    (is (= "12" (with-out-str (def ^{:a (print 1)} a1 (print 2)))))
    (is (= "112" (with-out-str (defonce ^{:a (print 1)} a2 (print 2)))))
    (is (= "1" (with-out-str (defonce ^{:a (print 1)} a2 (print 2))))))
  (testing "Meta is evaluated before expression"
    (is (= "123" (with-out-str ^{(print 2) (print 3)} {:b (print 1)})))
    (is (= "123" (with-out-str ^{(print 2) (print 3)} [:b (print 1)])))
    (is (= "123" (with-out-str ^{(print 2) (print 3)} #{:b (print 1)}))))
    (is (= "123" (with-out-str [^{:b ^{:b (print 3)} {:c (print 2)}} [(print 1) 5]]))))
