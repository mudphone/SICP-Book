(ns app.chapter1-2-2
  (:require
   [javelin.core :refer [defc]]))

;; Exercise 1.14

(defn first-denomination [kinds-of-coins]
  (case kinds-of-coins
    1 1
    2 5
    3 10
    4 25
    5 50))

(defn cc [amount kinds-of-coins]
  (cond
    (= amount 0) 1
    (or (< amount 0) (= kinds-of-coins 0)) 0
    :else (+ (cc amount (dec kinds-of-coins))
             (cc (- amount
                    (first-denomination kinds-of-coins))
                 kinds-of-coins))))

(defc cc-100-5 (cc 100 5))


;; Exercise 1.15

(defn cube [x] (* x x x))

(defn p' [x]
  (do
    (println "applying p'")
    (- (* 3 x) (* 4 (cube x)))))

(defn abs [n] (max n (- n)))

(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p' (sine (/ angle 3.0)))))

(defc sine-12-15 (sine 12.15))
