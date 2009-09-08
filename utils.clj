(ns euler.utils)

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

(defn factors [n divisors]
  (if (> n 1)
    (let [test-div (first divisors)]
      (if (zero? (rem n test-div))
        (lazy-seq (cons test-div (factors (/ n test-div) divisors)))
        (factors n (next divisors))))))

(defn prime-factors [n]
  (factors n (lazy-primes)))

(defn all-permutes [l]
  (if (seq? l)
    (let [val (first l)]
      (lazy-cat (map (partial cons val) (all-permutes (next l)))
                (all-permutes (next l))))
    '(())))

(defn proper-factors [n]
  (distinct (let [factors (prime-factors n)
                  to-div (fn [l] (reduce #(* %1 %2) 1 l))]
              (map to-div (all-permutes factors)))))

(defn digits[n]
  (if (not (zero? n))
    (let [next-dig (rem n 10)]
      (lazy-seq (cons next-dig
                      (digits (/ (- n next-dig) 10)))))))

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

