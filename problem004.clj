(ns euler.problem004
  (:use euler.utils))

;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is 9009 =
;; 91 × 99.
;;
;; Find the largest palindrome made from the product of two 3-digit
;; numbers.

(defn palindrome-n? [n]
  (let [digs (digits n)]
    (= digs (reverse digs))))

(defn max-palindrome-under [n msf]
  (let [pal (loop [m n]
              (if (and (>= m 100) (> (* m n) msf))
                (do
                  (if (palindrome-n? (* m n))
                    (* m n)
                    (recur (dec m))))))]
    (if (> n 100)
      (if (nil? pal)
        (recur (dec n) msf)
        (recur (dec n) (max pal msf)))
      msf)))

(defn solve []
  (max-palindrome-under 999 0))


