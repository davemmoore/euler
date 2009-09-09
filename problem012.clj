(ns euler.problem012 (:use euler.utils))

(defn solve []
  (let [primes (lazy-primes)
        num-divisors (fn [n] (count (proper-divisors n primes)))]
    (some #(if (> (first %) 500) %)
          (map #(list (num-divisors %) %) (triangle-numbers)))))
