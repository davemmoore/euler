(ns euler.problem022)

;; Using names.txt (right click and 'Save Link/Target As...'), a 46K
;; text file containing over five-thousand first names, begin by
;; sorting it into alphabetical order. Then working out the
;; alphabetical value for each name, multiply this value by its
;; alphabetical position in the list to obtain a name score.
;;
;; For example, when the list is sorted into alphabetical order,
;; COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name
;; in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
;;
;; What is the total of all the name scores in the file?

(def problem022-filename "d:/projects/euler/data/names_problem022.txt")

(defn- strip-quotes [str]
  (subs str 1 (dec (count str))))

(defn- name-score [pair]
  (let [name (first pair)
        pos  (fnext pair)
        name-sum (reduce + (map #(- (int %) (int \A) -1) name))]
    (if (== pos 938)
      (println name pos name-sum (* pos name-sum)))
    (* pos name-sum)))

(defn solve []
  (let [contents (slurp problem022-filename)
        names (sort (.split contents ","))
        pairs (map #(list (strip-quotes %1) %2) names (iterate inc 1))]
    (reduce + (map name-score pairs))))
