(ns euler.problem009)
;; A Pythagorean triplet is a set of three natural numbers, a < b < c,
;; for which,
;;     a^(2) + b^(2) = c^(2)
;;
;; For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
;;
;; There exists exactly one Pythagorean triplet for which a + b + c =
;; 1000. Find the product abc.

(defn- gen-valid-candidates [lt]
  (letfn [(pythag? [trip]
                   (let [[a b c] trip]
                     (= (+ (* a a) (* b b)) (* c c))))]
    (filter pythag?
            (for [a (range 1 (inc lt))
                  b (range a (inc lt))
                  c (range b (inc lt))
                  :while (<= (+ a b c) lt)]
              [a b c]))))

(defn solve []
  (letfn [(pythag-eq? [trip n]
                        (let [[a b c] trip]
                          (if (= (+ a b c) n)
                            (list a b c))))]
    (filter #(pythag-eq? % 1000) (gen-valid-candidates 1000))))
