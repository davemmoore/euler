(ns euler.problem030
  (:use euler.utils) (:use clojure.contrib.math))

(defn- fifth-sum? [n]
  (let [sum (reduce + (map #(expt % 5) (digits n)))]
    (== sum n)))

(defn- valid-list []
  (filter fifth-sum?
          (range 2 (* 5 (expt 9 5)))))

(defn solve []
  (reduce + (valid-list)))
