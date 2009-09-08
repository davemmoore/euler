(ns euler.problem015 (:use euler.utils))

;; Starting in the top left corner of a 2×2 grid, there are 6 routes
;; (without backtracking) to the bottom right corner.
;;
;; How many routes are there through a 20×20 grid?

(declare num-routes)

(defn- num-routes-internal [w h]
  (if (== w h 0)
    1
    (+ (if (> w 0)
         (num-routes (dec w) h)
         0)
       (if (> h 0)
         (num-routes w (dec h))
         0))))

(def num-routes (memoize num-routes-internal))

(defn solve []
  (num-routes 20 20))
