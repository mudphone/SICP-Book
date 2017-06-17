(ns app.chapter2-2-1
  (:require
   [javelin.core :refer [defc]]
   [app.pair :refer [append car' cdr' cons' deep-reverse
                     list' list-str map' null?' pair? reverse']]))

;; Exercise 2.17
(defn last-pair [x]
  (if (nil? (cdr' x))
    (car' x)
    (last-pair (cdr' x))))

(defc last-pair-list-123 (last-pair (list' 1 2 3)))

;; Exercise 2.18
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

;; Exercise 2.21
(defn sq [x] (* x x))

(defn square-list1 [items]
  (if (null?' items)
      nil
      (cons' (sq (car' items))
             (square-list1 (cdr' items)))))

(defn square-list2 [items]
  (map' sq items))

(defc sq-list1-1234 (list-str (square-list1 (list' 1 2 3 4))))
(defc sq-list2-1234 (list-str (square-list2 (list' 1 2 3 4))))

;; Exercise 2.22
(defn square-list3 [items]
  (let [go (fn [things answer]
             (if (null?' things)
               answer
               (recur (cdr' things)
                      (cons' (sq (car' things)) answer))))]
    (go (reverse' items) nil)))

(defc sq-list3-1234 (list-str (square-list3 (list' 1 2 3 4))))

;; Exercise 2.25
(defc seven1 (car' (cdr' (car' (cdr' (cdr' (list' 1 3 (list' 5 7) 9)))))))
(defc seven2 (car' (car' (list' (list' 7)))))
(defc seven3 (let [x (list' 1 (list' 2 (list' 3 (list' 4 (list' 5 (list' 6 7))))))]
               (car'
                (cdr'
                 (car'
                  (cdr'
                   (car'
                    (cdr'
                     (car'
                      (cdr'
                       (car'
                        (cdr'
                         (car'
                          (cdr' x))))))))))))))

;; Exercise 2.26
(def list-x (list' 1 2 3))
(def list-y (list' 4 5 6))
(defc append-lists (list-str (append list-x list-y)))
(defc cons-lists (list-str (cons' list-x list-y)))
(defc list-lists (list-str (list' list-x list-y)))

;; Exercise 2.27
(def tree-x (list' (list' 1 2) (list' 3 4)))
(defc deep-reverse-12-34 (list-str (deep-reverse tree-x)))

;; Exercise 2.28
(defn fringe [x]
  (cond
    (null?' x) nil
    (not (pair? x)) x

    (pair? (car' x))
    (fringe (append (car' x) (cdr' x)))
    
    :else (cons' (car' x)
                 (fringe (cdr' x)))))

(defc fringe-1-6 (list-str (fringe (list' (list' 1 (list' 2 3) 4) (list' 5 6)))))

