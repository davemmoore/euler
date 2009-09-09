(ns euler.problem023 (:use euler.utils))

;; A perfect number is a number for which the sum of its proper divisors
;; is exactly equal to the number. For example, the sum of the proper
;; divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
;; is a perfect number.
;;
;; A number n is called deficient if the sum of its proper divisors is
;; less than n and it is called abundant if this sum exceeds n.
;;
;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;; smallest number that can be written as the sum of two abundant numbers
;; is 24. By mathematical analysis, it can be shown that all integers
;; greater than 28123 can be written as the sum of two abundant
;; numbers. However, this upper limit cannot be reduced any further by
;; analysis even though it is known that the greatest number that cannot
;; be expressed as the sum of two abundant numbers is less than this
;; limit.
;;
;; Find the sum of all the positive integers which cannot be written as
;; the sum of two abundant numbers.

(defn- abundant-numbers []
  (let [primes (lazy-primes)
        ints   (range 1 28124)]
    (letfn [(abundant? [n]
                      (let [sum-factor (reduce + (proper-divisors n primes))]
                        (> sum-factor n)))]
      (filter abundant? ints))))

(defn- abundant-mask []
  (reduce #(bit-set %1 %2) 0 (abundant-numbers)))

(defn- shifted-masks [mask]
  (filter #(not (zero? %))
          (map #(if (zero? %1) 0 (bit-shift-left mask %2))
               (binary-digits mask)
               (iterate inc 0))))

(defn- create-possible-mask []
  (let [ab-mask (abundant-mask)
        shifts  (shifted-masks ab-mask)]
    (reduce bit-or 0 shifts)))

(defn solve []
  (let [possible (create-possible-mask)]
    (reduce + (take 28124 (map #(if (zero? %1) %2 0)
                               (binary-digits possible)
                               (iterate inc 0))))))
