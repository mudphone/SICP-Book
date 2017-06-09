(ns app.chapter2-1-3
  (:require [javelin.core :refer [defc]]))

;; Exercise 2.4
(defn cons1 [x y]
  (let [dispatch (fn [m]
                   (cond (= m 0) x
                         (= m 1) y
                         :else (println "Argument not 0 or 1 -- CONS" m)))]
    dispatch))

(defn car1 [z] (z 0))

(defn cdr1 [z] (z 1))

(defc car1-cons1-1-2 (car1 (cons1 1 2)))
(defc cdr1-cons1-1-2 (cdr1 (cons1 1 2)))

(defn cons2 [x y]
  (fn [m] (m x y)))

(defn car2 [z]
  (z (fn [p q] p)))

(defn cdr2 [z]
  (z (fn [p q] q)))

(defc car2-cons2-1-2 (car2 (cons2 1 2)))
(defc cdr2-cons2-1-2 (cdr2 (cons2 1 2)))

;; Exercise 2.5
(defn pair3 [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

;; (3, 2) = 8x9=72
;; 72/2 = 36
;; 36/2 = 18
;; 18/2 = 9

(defn cons3 [a b]
  (pair3 a b))

(defn divs
  "Remainder and number of times a number is divisible by a given
   divisor before the remainder is no longer evenly divisible."
  [x divisor]
  (let [divisible? (fn [n]
                     (= 0 (mod n divisor)))
        go (fn [n i]
             (if (divisible? (rem n divisor))
               (recur (quot n divisor) (inc i))
               [n i]))]
    (go x 0)))

(defn twos
  "Returns non-evenly divisible remainder and how many times
   two divides a number evenly (with a whole number remainder)."
  [x]
  (divs x 2))

(defn threes
  "Returns number of times three divides into a number before
   until the remainder is no longer divisible by three."
  [x]
  (divs x 3))

(defn car3 [z]
  (let [[_ x] (twos z)]
    x))

(defn cdr3 [z]
  (let [[_ y] (threes z)]
    y))

(defc car3-cons3-1-2 (car3 (cons3 1 2)))
(defc cdr3-cons3-1-2 (cdr3 (cons3 1 2)))

;; Exercise 2.6
(defn zero  [f] (fn [x] x))
(defn add-1 [n] (fn [f] (fn [x] (f ((n f) x)))))
(defn one [f] (fn [x] (f x)))
(defn two [f] (fn [x] (f (f x))))

(defc zero-inc-0 ((zero inc) 0))
(defc one-inc-0 ((one inc) 0))
(defc two-inc-0 ((two inc) 0))
(defc add-1-two-inc-0 (((add-1 two) inc) 0))
