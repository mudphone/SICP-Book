(ns app.chapter2-1-1
  (:require
   [app.rational :refer [make-rat numer denom rat-str]]
   [javelin.core :refer [defc defc= cell=]]))

;; Exercise 2.1
(defc make-rat-1-2 (rat-str (make-rat 1 2)))
(defc numer-rat-neg1-neg2 (numer (make-rat -1 -2)))
(defc denom-rat-neg1-neg2 (denom (make-rat -1 -2)))
(defc numer-rat-1-neg2 (numer (make-rat 1 -2)))
(defc denom-rat-1-neg2 (denom (make-rat 1 -2)))
