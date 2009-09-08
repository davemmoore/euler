(ns euler.problem005
  (:use euler.utils))

;; 2520 is the smallest number that can be divided by each of the
;; numbers from 1 to 10 without any remainder.
;;
;; What is the smallest number that is evenly divisible by all of the
;; numbers from 1 to 20?

(defn- merge-factors [l1 l2]
  (if (seq? l1)
    (if (seq? l2)
      (let [l1f (first l1)
            l2f (first l2)]
        (if (= l1f l2f)
          (cons l1f (merge-factors (next l1) (next l2)))
          (if (< l1f l2f)
            (cons l1f (merge-factors (next l1) l2))
            (cons l2f (merge-factors l1 (next l2))))))
      l1)
    (if (seq? l2)
      l2)))

(defn- get-lcm-factors [l]
  (if (not (nil? l))
    (merge-factors (prime-factors (first l))
                   (get-lcm-factors (next l)))))

(defn solve []
  (apply * (get-lcm-factors (map inc (range 20)))))

