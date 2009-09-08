(ns euler)

;; If we list all the natural numbers below 10 that are multiples of 3
;; or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
(defn solve-problem001 []
  (apply +
         (filter #(or (= 0 (mod %1 3))
                      (= 0 (mod %1 5)))
                 (map inc (range 999)))))