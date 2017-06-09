(ns app.chapter2-1-4
  (:require [javelin.core :refer [defc]]))

;; Exercise 2.7
(defn make-interval [a b] [a b])

(defn upper-bound [[_ b]] b)
(defn lower-bound [[a _]] a)

(defc upper-bound-1-2 (upper-bound (make-interval 1 2)))
(defc lower-bound-1-2 (lower-bound (make-interval 1 2)))

;; Exercise 2.8
(defn sub-interval [x y]
  (let [l (- (lower-bound x) (upper-bound y))
        u (- (upper-bound x) (lower-bound y))]
    (make-interval l u)))

(defn interval-str [x]
  (str "[" (lower-bound x) "," (upper-bound x) "]"))

(defc sub-interval1 (interval-str
                     (sub-interval (make-interval -10 -1) (make-interval 100 1000))))
(defc sub-interval2 (interval-str
                     (sub-interval (make-interval 100 1000) (make-interval -10 -1))))
(defc sub-interval3 (interval-str
                     (sub-interval (make-interval 100 1000) (make-interval 1 2))))

;; Exercise 2.9
(defn abs [x] (if (neg? x) (- x) x))

(defn width-interval [x]
  (-> (- (upper-bound x) (lower-bound x))
      abs
      (/ 2.0)))

(defc width1a (width-interval (make-interval -10 -1)))
(defc width1b (width-interval (make-interval 100 1000)))
(defc width1s (width-interval (sub-interval
                               (make-interval -10 -1)
                               (make-interval 100 1000))))

(defc width2a (width-interval (make-interval 100 1000)))
(defc width2b (width-interval (make-interval -10 -1)))
(defc width2s (width-interval (sub-interval
                               (make-interval 100 1000)
                               (make-interval -10 -1))))

(defc width3a (width-interval (make-interval 100 1000)))
(defc width3b (width-interval (make-interval 1 2)))
(defc width3s (width-interval (sub-interval
                               (make-interval 100 1000)
                               (make-interval 1 2))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defc width-m1a (width-interval (make-interval -10 -1)))
(defc width-m1b (width-interval (make-interval 100 1000)))
(defc width-m1 (width-interval
                (mul-interval (make-interval -10 -1)
                              (make-interval 100 1000))))

(defc width-m2a (width-interval (make-interval 100 1000)))
(defc width-m2b (width-interval (make-interval -10 -1)))
(defc width-m2 (width-interval
                (mul-interval (make-interval 100 1000)
                              (make-interval -10 -1))))

(defc width-m3a (width-interval (make-interval 100 1000)))
(defc width-m3b (width-interval (make-interval 1 2)))
(defc width-m3 (width-interval
                (mul-interval (make-interval 100 1000)
                              (make-interval 1 2))))

;; Exercise 2.10
(defn spans-zero? [x]
  (and
   (>= 0 (lower-bound x))
   (<= 0 (upper-bound x))))

(defn div-interval [x y]
  (if (spans-zero? y)
    (println "Error: div by zero")
    (mul-interval x 
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
