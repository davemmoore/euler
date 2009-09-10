(ns euler.problem035 (:use euler.utils))

(defn- next-rotation [n]
  (let [digs (reverse (digits n))
        new-ones (first digs)
        rest (reduce #(+ (* %1 10) %2) 0 (rest digs))]
    (+ (* 10 rest) new-ones)))

(defn- prime-mask [below]
  (reduce #(bit-set %1 %2) 0 (take-while #(< % 1000000)
                                         (lazy-primes))))

