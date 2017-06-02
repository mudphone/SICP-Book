(ns app.chapter1-2-5
  (:require [javelin.core :refer [defc]]))

;; Exercise 1.20

(defn remainder [a b] (mod a b))

(defn gcd [a b]
  (if (= b 0)
      a
      (recur b (remainder a b))))
