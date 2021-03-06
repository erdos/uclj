#!/usr/bin/env clojure

(require '[clojure.java.shell :refer [sh]])

(def scripts-dir "/home/erdos/Work/advent-of-code/2021")

(def script-files
  [["day1.clj" 10]
   ["day2.clj" 10]
   ["day3.clj" 10]
   ["day4.clj" 10]
   ["day5.clj" 10]
   ["day6.clj" 10]
   ["day7.clj" 10]
   ["day8.clj" 10]
   ["day9.clj" 10]
   ["day11.clj" 10]
   ["day17.clj" 10]
   ["day19.clj" 10]
   ["day24.clj" 10]])

(def runners ["uclj" "bb" "clojure"])

(defmacro measure [body]
  `(let [before# (System/currentTimeMillis)
         result# ~body
         after# (System/currentTimeMillis)]
     [result# (- after# before#)]))

(defn run-item! [runner file]
  (doto (measure (sh runner (str scripts-dir "/" file) :dir scripts-dir))
    (-> first :exit zero? assert)))

(defn stats [numbers]
  (let [n     (count numbers)
        mean  (double (/ (reduce + numbers) n))
        stdev (Math/sqrt (/ (reduce + (for [a numbers] (Math/pow (- mean a) 2)))
                            (dec n)))]
    [mean stdev]))

(defn format-stat [[mean stdev]]
  (format "%d+%d" (Math/round mean) (Math/round stdev)))

(defn pritems [& xs]
  (apply printf (str (apply str (repeat (count xs) "| %-10s ")) "\n") xs)
  (flush))

(apply pritems "test case" runners)

(def warmup-count 1)

(doseq [[file repeat-count] script-files]
  (apply pritems file
         (for [runner runners]
           (format-stat (stats (drop warmup-count (repeatedly (+ warmup-count repeat-count) #(second (run-item! runner file)))))))))

(shutdown-agents)
