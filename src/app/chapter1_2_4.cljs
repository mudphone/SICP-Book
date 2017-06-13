(ns app.chapter1-2-4
  (:require [javelin.core :refer [defc]]))

;; Exercise 1.16

(defn square [x] (* x x))

(defn expt [b n]
  (cond (= n 0) 1
        (even? n) (expt (square b) (/ n 2))
        :else (* b (expt b (dec n)))))

(defc expt-2-3 (expt 2 3))

;; Exercise 1.17
(defn dbl [x] (* 2 x))
(defn halve [x] (/ x 2))

(defn my-* [a b]
  (cond (= b 0) 0
        (even? b) (my-* (dbl a) (halve b))
        :else (+ a (my-* a (dec b)))))

(defc my-*-2-7 (my-* 2 7))

;; Exercise 1.18
(defn it-*-iter [a b acc]
  (cond (= b 0) 0
        (= b 1) (+ a acc)
        (even? b) (it-*-iter (dbl a) (halve b) acc)
        :else (it-*-iter a (dec b) (+ acc a))))

(defn it-* [a b]
  (it-*-iter a b 0))

(defc it-*-3-5 (it-* 3 5))

;; Exercise 1.19
(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count) (fib-iter a
                                b
                                (+ (square p) (square q))
                                (+ (* 2 p q) (square q))
                                (/ count 2))
        :else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))))

(defn fib [n]
  (fib-iter 1 0 0 1 n))

(defc clever-fib-6 (fib 6))
