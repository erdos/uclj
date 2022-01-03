(def scripts-dir "/home/erdos/Work/advent-of-code/2021")

(def script-files [;"day1.clj" "day2.clj" "day3.clj" "day4.clj" "day5.clj" "day6.clj" "day7.clj" "day8.clj" "day9.clj"
                   "day24.clj"])

(def runners ["bb" "/home/erdos/Work/uclj/uclj"])

(defmacro measure [body]
  `(let [before# (System/currentTimeMillis)
         result# ~body
         after# (System/currentTimeMillis)]
     [result# (- after# before#)]))

(doseq [file script-files]
  (doseq [runner runners]
    (println "Running" file "with" runner)
    (let [[result runtime] (measure (clojure.java.shell/sh
                                     runner
                                     (str scripts-dir "/" file)
                                     :dir scripts-dir))]
      (assert (zero? (:exit result)) (str "Error " (pr-str result)))
      (println " - Runtime was" runtime "ms"))))
