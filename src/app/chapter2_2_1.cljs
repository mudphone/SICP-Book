(ns app.chapter2-2-1
  (:require
   [javelin.core :refer [defc]]
   [app.pair :refer [car' cdr' cons' list' list-str]]))

;; Exercise 2.17
(defn last-pair [x]
  (if (nil? (cdr' x))
    (car' x)
    (last-pair (cdr' x))))

(defc last-pair-list-123 (last-pair (list' 1 2 3)))

;; Exercise 2.18 
(defn reverse' [xs]
  (let [go (fn [acc r]
             (if (nil? r)
               acc
               (recur (cons' (car' r) acc) (cdr' r))))]
    (go nil xs)))

(defc reverse-list-123 (list-str (reverse' (list' 1 2 3))))
