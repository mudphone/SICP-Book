(ns app.chapter2-2-3
  (:require
   [javelin.core :refer [defc]]
   [app.pair :refer [accumulate accumulate-n cons' enumerate-interval
                     enumerate-tree filter' fold-left fold-right
                     list' list-n list-str map' map-n pair?]]))

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
(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(defc horner-eval-130501 (horner-eval 2 (list' 1 3 0 5 0 1)))

;; Exercise 2.35
#_(defn accumulate [op initial sequence]
  (if (null?' sequence)
      initial
      (op (car' sequence)
          (accumulate op initial (cdr' sequence)))))

(defn count-leaves [t]
  (accumulate + 0 (map' (fn [x]
                          (if (pair? x)
                            (count-leaves x)
                            1))
                        t)))

(defc count-leaves-4 (count-leaves (list' 1 (list' 2 3) 4)))

;; Exercise 2.36
(def seq-seqs (list' (list' 1 2 3) (list' 4 5 6) (list' 7 8 9) (list' 10 11 12)))

(defc accumulate-n-seq-seqs (list-str (accumulate-n + 0 seq-seqs)))

;; Exercise 2.37
(defn dot-product [v w]
  (accumulate + 0 (map-n * v w)))

(defc dot-product-123-456 (dot-product (list' 1 2 3) (list' 4 5 6)))

(defn matrix-*-vector [m v]
  (map' (fn [m-row]
          (dot-product m-row v))
        m))

(def matrix (list' (list' 1 2 3 4) (list' 5 6 7 8) (list' 9 10 11 12)))

(defc m-by-v (list-str (matrix-*-vector matrix (list' 2 3 4 5))))

(defn transpose [mat]
  (accumulate-n cons' nil mat))

(defc transpose-matrix (list-str (transpose matrix)))

(defn matrix-*-matrix [m n]
  (let [n-cols (transpose n)]
    (map' (fn [m-row] 
            (map' (fn [n-col]  
                    (dot-product m-row n-col))  
                  n-cols))
          m)))
(defc m-by-m (list-str (matrix-*-matrix matrix matrix)))

;; Exercise 2.38
(defc fr-d-1-123 (fold-right / 1 (list' 1 2 3)))
(defc fl-d-1-123 (fold-left / 1 (list' 1 2 3)))
(defc fr-l-nil-123 (list-str (fold-right list' nil (list' 1 2 3))))
(defc fl-l-nil-123 (list-str (fold-left list' nil (list' 1 2 3))))
