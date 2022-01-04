#!/usr/bin/env clojure

(require '[clojure.java.shell :refer [sh]])

(def scripts-dir "/home/erdos/Work/advent-of-code/2021")

(def script-files ["day1.clj" "day2.clj" "day3.clj" "day4.clj" "day5.clj" "day6.clj" #_"day7.clj" "day8.clj" "day9.clj"
                   "day24.clj"])

(def runners ["uclj" "bb" "clojure"])

(defmacro measure [body]
  `(let [before# (System/currentTimeMillis)
         result# ~body
         after# (System/currentTimeMillis)]
     [result# (- after# before#)]))

(apply println "test case" (interleave (repeat \tab) runners))

(doseq [file script-files]
  (apply println
         file
         (interleave (repeat \tab)
                     (for [runner runners]
                       (let [[result runtime] (measure (sh
                                                        runner
                                                        (str scripts-dir "/" file)
                                                        :dir scripts-dir))]
                         (assert (zero? (:exit result)) (str "Error " (pr-str result)))
                         #_(println " - Runtime was" runtime "ms")
                         runtime)))))
