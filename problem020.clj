(ns euler.problem020 (:use euler.utils))

;; n! means n x (n - 1) x ... x 3 x 2 x 1
;;
;; Find the sum of the digits in the number 100!

(defn- factorial [n]
  (apply * (range 1 (inc n))))

(defn solve []
  (apply + (digits (factorial 100))))
