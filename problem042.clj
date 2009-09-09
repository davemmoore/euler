(ns euler.problem042 (:use euler.utils))

(def problem042-filename "d:/projects/euler/data/words_problem042.txt")

(defn- triangle-numbers []
  (letfn [(tn [n] (/ (* n (inc n)) 2))]
    (map tn (iterate inc 1))))

(defn- triangle-mask [n]
  (reduce #(bit-set %1 %2) 0 (take n (triangle-numbers))))

(defn solve []
  (let [tri-mask100 (triangle-mask 100)
        names (slurp-euler-words problem042-filename)]
    (count (filter #(bit-test tri-mask100 (word-score %)) names))))
