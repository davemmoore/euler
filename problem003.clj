(ns euler.problem003 (:use euler.utils))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143?

(defn solve []
  (reduce max (factors 600851475143 (lazy-primes))))
