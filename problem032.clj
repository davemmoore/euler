(ns euler.problem032
  (:use euler.utils) (:use clojure.set))

;; We shall say that an n-digit number is pandigital if it makes use of
;; all the digits 1 to n exactly once.  For example, the 5-digit number,
;; 15234, is 1 through 5 pandigital.
;;
;; The product 7254 is unusual, as the identity, 39 × 186 = 7254,
;; containing multiplicand, multiplier, and product is 1 through 9
;; pandigital.
;;
;; Find the sum of all products whose multiplicand/multiplier/product
;; identity can be written as a 1 through 9 pandigital.  HINT: Some
;; products can be obtained in more than one way so be sure to only
;; include it once in your sum.

;;; dmmnotes: Note that only 4 and 5 digit numbers are candiates here,
;;; since anything other than that puts too much "load" on the
;;; multiplication side.

(def problem032-digit-set '(1 2 3 4 5 6 7 8 9))
(def problem032-num-permutes (factorial 9))

(defn- n-from-digs [digs]
  (if (seq? digs)
    (+ (first digs) (* 10 (n-from-digs (next digs))))
    0))

(defn- check-equal-pos [perm pos]
  (let [n   (n-from-digs (take pos perm))
        rem (drop pos perm)]
    (loop [multiplier   (take 1 rem)
           multiplicand (drop 1 rem)]
      (if (not (empty? multiplicand))
        (let [lier (n-from-digs multiplier)
              cand (n-from-digs multiplicand)]
          (if (== (* lier cand) n)
            n
            (recur (reverse (cons (first multiplicand) (reverse multiplier)))
                   (next multiplicand))))))))

(defn- candidate-check [perm]
  (or (check-equal-pos perm 4)
      (check-equal-pos perm 5)
      0))

;; Note that this does extra work, since we consider each permutation
;; with the multiplier/multiplicand in both orderings, but eh.  It
;; still lives by the one minute rule.
(defn solve []
  (reduce + (distinct
             (filter #(not (zero? %))
                     (map candidate-check
                          (map (partial nth-permute problem032-digit-set)
                               (range problem032-num-permutes)))))))
