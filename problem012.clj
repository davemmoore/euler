(ns euler.problem012 (:use euler.utils))

;; returns a lazy-seq of (next-tri ct) (gen-tri-recur (next-tri ct))
(defn- gen-triangles-recur [ct]
  (let [get-next (fn [curr]
                   (let [[ct i] curr]
                     [(+ ct i 1) (inc i)]))
        next-tri (get-next ct)]
    (lazy-seq (cons next-tri (gen-triangles-recur next-tri)))))

(defn gen-triangles []
  (map first (lazy-seq (cons [1 1] (gen-triangles-recur [1 1])))))

(defn- num-divisors [n]
  (count (proper-factors n)))

(defn solve []
  (some #(if (> (first %) 2000) %)
        (map #(list (num-divisors %) %) (gen-triangles))))


(defn triangle-factors [n]
  (take n (map prime-factors (gen-triangles))))
