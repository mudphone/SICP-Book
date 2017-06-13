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

;; Exercise 2.19
(def us-coins (list' 50 25 10 5 1))
(def uk-coins (list' 100 50 20 10 5 2 1 0.5))

(def first-denomination car')

(def except-first-denomination cdr')

(def no-more? nil?)

(defn cc [amount coin-values]
  (cond
    (= amount 0) 1
    (or (< amount 0) (no-more? coin-values)) 0
    :else (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values))))

(defc cc-100-us-coins (cc 100 us-coins))

;; Exercise 2.20
(defn same-parity [& xs]
  (if (even? (first xs))
    (filter even? xs)
    (filter odd? xs)))

(defc same-parity-odd (same-parity 1 2 3 4 5 6 7))
(defc same-parity-even (same-parity 2 3 4 5 6 7))
