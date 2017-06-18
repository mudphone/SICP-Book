(ns app.chapter2-2-3
  (:require
   [javelin.core :refer [defc]]
   [app.pair :refer [accumulate cons' enumerate-interval
                     enumerate-tree filter' list' list-n
                     list-str]]))

;; Exercise 2.33
(defn map-accumulate [p sequence]
  (accumulate (fn [x y] (cons' (p x) y)) nil sequence))

(defn sq [x] (* x x))

(defc map-accumulate-sq (list-str (map-accumulate sq (list' 1 2 3 4 5))))

(defn append-accumulate [seq1 seq2]
  (accumulate cons' seq2 seq1))

(defc append-accumulate-12-34 (list-str (append-accumulate (list' 1 2) (list' 3 4))))

(defn length-accumulate [sequence]
  (accumulate (fn [_ acc] (inc acc)) 0 sequence))

(defc length-accumulate-5 (str (length-accumulate (list' 1 3 4 2 1))))

;; Exercise 2.34
#_(defn accumulate [op initial sequence]
  (if (null?' sequence)
      initial
      (op (car' sequence)
          (accumulate op initial (cdr' sequence)))))

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(defc horner-eval-130501 (horner-eval 2 (list' 1 3 0 5 0 1)))
