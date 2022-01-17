(ns clojure-core-test
  (:require [clojure.data.xml]
            [clojure.test :refer :all]))

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

(deftest test-arrays
  ;; Long[][][][]... is not supported by the closed-world assumption
  (is (every? some? (take 10 (iterate (fn [x] (into-array [x])) 3))))
  (testing "aclone"
    (let [a (int-array [1 2 3 4])
          b (aclone a)]
      (is (every? integer? b))
      (is (= 1 (aget a 0) (aget b 0)))
      (testing "aget-aset"
      (aset-int b 0 2)
      (is (= 2 (aget b 0)))))))

(deftest test-xml-lib
  (is (map? (clojure.data.xml/parse-str "<a>1</a>"))))
