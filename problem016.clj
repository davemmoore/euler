(ns euler.problem016
  (:use euler.utils)
  (:use clojure.contrib.math))

(defn solve []
  (apply + (digits (expt 2 1000))))
