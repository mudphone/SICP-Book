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
