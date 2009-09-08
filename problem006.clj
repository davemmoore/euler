(ns euler.problem006
  (:use euler.utils))

;; The sum of the squares of the first ten natural numbers is,
;;     1^(2) + 2^(2) + ... + 10^(2) = 385
;;
;; The square of the sum of the first ten natural numbers is,
;;     (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
;;
;; Hence the difference between the sum of the squares of the first
;; ten natural numbers and the square of the sum is 3025 | 385 = 2640.
;;
;; Find the difference between the sum of the squares of the first one
;; hundred natural numbers and the square of the sum.

(defn- natural-to [n] (range 1 (inc n)))

(defn solve []
  (let [sumsq (apply + (map #(* % %) (natural-to 100)))
        sqsum ((fn [n] (* n n)) (apply + (natural-to 100)))]
    (- sqsum sumsq)))
