(ns euler.problem017 (:use euler.utils))

;; If the numbers 1 to 5 are written out in words: one, two, three,
;; four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
;; total.
;;
;; If all the numbers from 1 to 1000 (one thousand) inclusive were
;; written out in words, how many letters would be used?
;;
;; NOTE: Do not count spaces or hyphens. For example, 342 (three
;; hundred and forty-two) contains 23 letters and 115 (one hundred and
;; fifteen) contains 20 letters. The use of "and" when writing out
;; numbers is in compliance with British usage.

(defn- num-less-than-10 [n]
  (if (or (< n 0) (> n 9))
    (throw (IllegalArgumentException. "n must be between 1 and 9")))
  (let [nums ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]]
    (nth nums n)))

(defn- num-lequal-20 [n]
  (if (or (< n 1) (> n 20))
    (throw (IllegalArgumentException. "n must be between 1 and 20")))
  (if (< n 10)
    (num-less-than-10 n)
    (let [nums [0 1 2 3 4 5 6 7 8 9
                "ten" "eleven" "twelve" "thirteen" "fourteen"
                "fifteen" "sixteen" "seventeen" "eighteen"
                "nineteen" "twenty"]]
      (nth nums n))))

(defn- lookup-tens [n]
  (if (or (< n 20) (> n 90) (not (zero? (rem n 10))))
    (throw (IllegalArgumentException. "n must be between 30 and 90, and evenly divisible by ten")))
  (let [ten-names ["twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]
        idx (- (/ n 10) 2)]
    (nth ten-names idx)))

(defn- lookup-hundreds [n]
  (if (or (< n 100) (> n 1000) (not (zero? (rem n 100))))
    (throw (IllegalArgumentException. "n must be between 100 and 1000, and evenly divisible by 100")))
  (if (== n 1000)
    "onethousand"
    (let [hundreds (/ n 100)]
      (str (num-less-than-10 hundreds) "hundred"))))

(defn- num-less-than-100 [n]
  (if (<= n 20)
    (num-lequal-20 n)
    (let [ones (rem n 10)
          tens (- n ones)]
      (str (lookup-tens tens) (num-less-than-10 ones)))))

(defn- number-to-string [n]
  (if (< n 100)
    (num-less-than-100 n)
    (let [mod (rem n 100)
          hundies (- n mod)]
      (str (lookup-hundreds hundies)
           (if (not (== mod 0))
             (str "and" (num-less-than-100 mod)))))))

(defn solve []
  (reduce + (map count (map number-to-string (range 1 1001)))))
