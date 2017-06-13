(ns app.chapter1-1
  (:require
   [javelin.core :refer [cell cell=]]))

;; Exercise 1.1
(def ex1-1a (cell 10))
(def ex1-1b (cell (+ 5 3 4)))
(def ex1-1c (cell (- 9 1)))
(def ex1-1d (cell (/ 6 2)))
(def ex1-1e (cell (+ (* 2 4) (- 4 6))))
(def xa (cell 3))
(def xb (cell= (+ xa 1)))
;; (def ex1-1f (cell= xa))
;; (def ex1-1g (cell= xb))
(def ex1-1h (cell= (+ xa xb (* xa xb))))
(def ex1-1i (cell= (= xa xb)))
(def ex1-1j (cell= (if (and (> xb xa) (< xb (* xa xb)))
                     xb
                     xa)))
(def ex1-1k (cell= (cond (= xa 4) 6
                         (= xb 4) (+ 6 7 xa)
                         :else 25)))
(def ex1-1l (cell= (+ 2 (if (> xb xa)
                          xb
                          xa))))
(def ex1-1m (cell= (* (cond (> xa xb) xa
                            (< xa xb) xb
                            :else -1)
                      (+ xa 1))))

;; Exercise 1.2
(def ex1-2 (cell (/ (+ 5 (/ 1 4) (- 2 (- 3 (+ 6 (/ 1 3)))))
                    (* 3 (- 6 2) (- 2 7)))))

;; Exercise 1.3
(defn square [x] (* x x))
(defn sum-of-squares [x y]
  (+ (square x) (square y)))
(defn high-sum-of-squares [x y z]
  (apply sum-of-squares (drop 1 (sort [x y z]))))
(def ex1-3 (cell (high-sum-of-squares 1 3 5)))

;; Exercise 1.7
(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn abs [x]
  (if (> x 0) x (* -1 x)))

(defn pct-change [x y]
  (/ (abs (- x y)) y))

(defn good-enough? [last-guess guess x]
  (< (pct-change last-guess guess) 0.001))

(defn sqrt-iter
  ([guess x]
   (sqrt-iter (* 2.0 guess) guess x))
  ([last-guess guess x]
   (if (good-enough? last-guess guess x)
     guess
     (sqrt-iter guess (improve guess x) x))))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; Exercise 1.8
(defn improve-cube [guess x]
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(defn cube-rt-iter
  ([guess x]
   (cube-rt-iter (* 2.0 guess) guess x))
  ([last-guess guess x]
   (if (good-enough? last-guess guess x)
     guess
     (cube-rt-iter guess (improve-cube guess x) x))))

(defn cube-rt [x]
  (cube-rt-iter 1.0 x))

