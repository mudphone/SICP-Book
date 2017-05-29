(ns app.chapter1-2-6
  (:require [javelin.core :refer [defc]]))

;; Exercise 1.21

(defn square [x] (* x x))

(defn remainder [a b] (mod a b))

(defn divides? [a b]
  (= (remainder b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defc smallest-divisor-199   (smallest-divisor 199))
(defc smallest-divisor-1999  (smallest-divisor 1999))
(defc smallest-divisor-19999 (smallest-divisor 19999))

;; Exercise 1.22
(defn prime? [n]
  (= n (smallest-divisor n)))

(defn runtime []
  (.getTime (js/Date.)))

(defn report-prime [n elapsed-time]
  (println " *** finding prime" n "took:" elapsed-time))

(defn start-prime-test [n start-time]
  (print "started at: " start-time "\n")
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))

(defn timed-prime-test [n]
  (start-prime-test n (runtime)))

(defn search-for-primes [a b]
  (println "searching from" a "to" b)
  (doseq [n (filter odd? (range a (inc b)))]
    (print "searching for" n " ")
    (timed-prime-test n)))

;; Exercise 1.23
(defn next-divisor [n]
  (if (= n 2)
    3
    (+ 2 n)))

(defn find-divisor2 [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor2 n (next-divisor test-divisor))))

(defn smallest-divisor2 [n]
  (find-divisor2 n 2))

(defn prime2? [n]
  (= n (smallest-divisor2 n)))

(defn start-prime-test2 [n start-time]
  (print "started at: " start-time "\n")
  (if (prime2? n)
    (report-prime n (- (runtime) start-time))))

(defn timed-prime-test2 [n]
  (start-prime-test2 n (runtime)))

(defn search-for-primes' []
  (doseq [n [1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037 10000019 10000079 10000103]]
    (timed-prime-test n)))

(defn search-for-primes2 []
  (doseq [n [1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037 10000019 10000079 10000103]]
    (timed-prime-test2 n)))

;; Exercise 1.24
(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
        (remainder (square (expmod base (/ exp 2) m))
                   m)
        :else
        (remainder (* base (expmod base (- exp 1) m))
                   m)))

(defn fermat-test [n]
  (let [try-it (fn [a]
                 (= (expmod a n n) a))]
    (try-it (inc (rand-int (dec n))))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (dec times))
        :else false))

(defn start-prime-test3 [n start-time]
  (print "started at: " start-time "\n")
  (if (fast-prime? n 10)
    (report-prime n (- (runtime) start-time))))

(defn timed-prime-test3 [n]
  (start-prime-test3 n (runtime)))

(defn search-for-primes3 []
  (doseq [n [1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037 10000019 10000079 10000103]]
    (timed-prime-test3 n)))

;; Exercise 1.27
(defn all-congruent? [n]
  (let [congruent? (fn [a]
                     (= (expmod a n n) (remainder a n)))]
    (every? congruent? (range 1 n))))

(def carmichael-numbers [561 1105 1729 2465 2821 6601])

(defn all-fooled? []
  (every? all-congruent? carmichael-numbers))
