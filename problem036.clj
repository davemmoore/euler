(ns euler.problem036 (:use euler.utils))

(defn solve []
  (apply + (let [candidates (map #(inc (* % 2)) (range 500000))]
             (filter #(and (palindrome-n? %)
                           (binary-palindrome-n? %)) candidates))))

