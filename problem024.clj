(ns euler.problem024 (:use euler.utils))

;; A permutation is an ordered arrangement of objects. For example,
;; 3124 is one possible permutation of the digits 1, 2, 3 and 4. If
;; all of the permutations are listed numerically or alphabetically,
;; we call it lexicographic order. The lexicographic permutations of
;; 0, 1 and 2 are:
;;
;; 012   021   102   120   201   210
;;
;; What is the millionth lexicographic permutation of the digits
;; 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

(defn- nth-permute [l n]
  (let [size (count l)]
    (if (== size 1)
      l
      (let [fact  (factorial (dec size))
            sel   (int (/ n fact))
            next  (- n (* sel fact))
            val   (nth l sel)
            rem   (lazy-cat (take sel l) (drop (inc sel) l))]
        (cons val (nth-permute rem next))))))

(defn solve []
  (nth-permute '(0 1 2 3 4 5 6 7 8 9) (dec 1000000)))



