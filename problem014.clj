(ns euler.problem014 (:use euler.utils))

;; The following iterative sequence is defined for the set of positive integers:
;;
;; n: n/2 (n is even)
;; n: 3n + 1 (n is odd)
;;
;; Using the rule above and starting with 13, we generate the following sequence:
;; 13 - 40 - 20 - 10 - 5 - 16 - 8 - 4 - 2 - 1
;;
;; It can be seen that this sequence (starting at 13 and finishing at
;; 1) contains 10 terms. Although it has not been proved yet (Collatz
;; Problem), it is thought that all starting numbers finish at 1.
;;
;; Which starting number, under one million, produces the longest chain?
;;
;; NOTE: Once the chain starts the terms are allowed to go above one million.

(declare collatz-len)

(defn- collatz-len-mem [n]
  (if (== n 1)
    1
    (if (even? n)
      (inc (collatz-len (/ n 2)))
      (inc (collatz-len (inc (* n 3)))))))

(def collatz-len (memoize collatz-len-mem))

(defn solve []
  (let [lens (map #(list (mem-collatz-len %) %) (range 2 1000000))
        select-max (fn [a b]
                     (if (> (first a) (first b)) a b))]
    (reduce select-max lens)))
