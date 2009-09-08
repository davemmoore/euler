(ns euler.problem010 (:use euler.utils))

;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;
;; Find the sum of all the primes below two million.

(defn solve []
  (apply + (take-while #(< % 2000000) (lazy-primes))))
