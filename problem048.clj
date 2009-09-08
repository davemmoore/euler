(ns euler.problem048
  (:use euler.utils)
  (:use clojure.contrib.math))

(defn solve []
  (let [n (reduce + (map #(expt % %) (range 1 1001)))]
    (reverse (take 10 (digits n)))))
