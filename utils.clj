(ns euler.utils)

(defn triangle-numbers []
  (letfn [(tn [n] (/ (* n (inc n)) 2))]
    (map tn (iterate inc 1))))

(defn word-score [name]
  (reduce + (map #(- (int %) (int \A) -1) name)))

(defn- strip-quotes [str] (subs str 1 (dec (count str))))

;; format "STRING","STRING","STRING"
(defn slurp-euler-words [filename]
  (map strip-quotes (.split (slurp filename) ",")))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

;; todo: understand this.
(defn lazy-primes []
  (letfn [(enqueue [sieve n factor]
                   (let [m (+ n factor)]
                     (assoc sieve m
                            (conj (sieve m) factor))))
          (next-sieve [sieve candidate]
                      (if-let [factors (sieve candidate)]
                        (reduce #(enqueue %1 candidate %2)
                                (dissoc sieve candidate)
                                factors)
                        (enqueue sieve candidate candidate)))
          (next-primes [sieve candidate]
                       (if (sieve candidate)
                         (recur (next-sieve sieve candidate) (inc candidate))
                         (cons candidate
                               (lazy-seq (next-primes (next-sieve sieve candidate)
                                                      (inc candidate))))))]
    (lazy-seq (next-primes {} 2))))

(defn nth-permute [l n]
  (let [size (count l)]
    (if (== size 1)
      l
      (let [fact  (factorial (dec size))
            sel   (quot n fact)
            next  (- n (* sel fact))
            val   (nth l sel)
            rem   (lazy-cat (take sel l) (drop (inc sel) l))]
        (cons val (nth-permute rem next))))))

(defn all-permutes [l]
  (if (and (seq? l) (not (empty? l)))
    (let [val (first l)]
      (lazy-cat (map (partial cons val) (all-permutes (next l)))
                (all-permutes (next l))))
    '(())))


(def prime-factors
     (letfn [(factors [n divisors sofar]
                      (if (> n 1)
                        (let [test-div (first divisors)]
                          (if (zero? (rem n test-div))
                            (recur (/ n test-div) divisors (cons test-div sofar))
                            (recur n (next divisors) sofar)))
                        sofar))]
       (fn this
         ([n] (factors n (lazy-primes) '()))
         ([n primes] (factors n primes '())))))

;; A bit hack-tastic, in that we know that the first element after distinct will be n
(def proper-divisors
     (letfn [(propers [n primes]
                      (if (zero? n)
                        '()
                        (let [full-list (distinct (let [factors (prime-factors n primes)
                                                        to-div (fn [l] (reduce #(* %1 %2) 1 l))]
                                                    (map to-div (all-permutes factors))))]
                          (assert (== (first full-list) n))
                          (next full-list))))]
       (fn this
         ([n]        (propers n (lazy-primes)))
         ([n primes] (propers n primes)))))

(defn digits[n]
  (if (not (zero? n))
    (let [next-dig (rem n 10)]
      (lazy-seq (cons next-dig
                      (digits (quot n 10)))))))

(defn binary-digits[n]
  (if (not (zero? n))
    (let [next-dig (rem n 2)]
      (lazy-seq (cons next-dig
                      (binary-digits (/ (- n next-dig) 2)))))))

(defn gen-fibonacci []
  (letfn [(fib-gen [x]
                   [(+ (first x) (second x)) (first x)])]
    (map first (iterate fib-gen [1 0]))))

(defn palindrome-n? [n]
  (let [digs (digits n)]
    (= digs (reverse digs))))

(defn binary-palindrome-n? [n]
  (let [digs (binary-digits n)]
    (= digs (reverse digs))))

