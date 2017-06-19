(ns app.chapter2-2-3
  (:require
   [javelin.core :refer [defc]]
   [app.chapter1-2-6 :refer [prime?]]
   [app.pair :refer [accumulate accumulate-n append car' cadr' caddr'
                     cons' enumerate-interval enumerate-tree
                     filter' flatmap fold-left fold-right
                     list' list-n list-str map' map-n pair?
                     remove']]))

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

;; Exercise 2.39
(defn rev-fr [sequence]
  (fold-right (fn [x y]
                (append y (list' x))) nil sequence))

(defc rev-fr-123 (list-str (rev-fr (list' 1 2 3))))

(defn rev-fl [sequence]
  (fold-left (fn [x y]
               (cons' y x)) nil sequence))

(defc rev-fl-123 (list-str (rev-fl (list' 1 2 3))))

;; Exercise 2.40
(defn unique-pairs [n]
  (flatmap (fn [i]
             (map' (fn [j] (list' i j))
                   (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(defc unique-pairs-3 (list-str (unique-pairs 3)))

(defn make-pair-sum [pair]
  (list' (car' pair) (cadr' pair) (+ (car' pair) (cadr' pair))))

(defn prime-sum? [pair]
  (prime? (+ (car' pair) (cadr' pair))))

(defn prime-sum-pairs [n]
  (map' make-pair-sum
        (filter' prime-sum?
                 (flatmap
                  (fn [i]
                    (map' (fn [j] (list' i j))
                          (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)))))

(defn prime-sum-pairs2 [n]
  (map' make-pair-sum
        (filter' prime-sum?
                 (unique-pairs n))))

(defc prime-sum-pairs-6  (list-str (prime-sum-pairs  6)))
(defc prime-sum-pairs2-6 (list-str (prime-sum-pairs2 6)))

;; Exercise 2.41
(defn unique-triples [n]
  (flatmap (fn [i]
             (flatmap (fn [j]
                        (map' (fn [k] (list' i j k))
                              (enumerate-interval 1 (dec j))))
                      (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(defn is-triple-sum? [target]
  (fn [triple]
    (= (+ (car' triple) (cadr' triple) (caddr' triple))
       target)))

(defn ordered-triples [n target-sum]
  (filter' (is-triple-sum? target-sum)
           (unique-triples n)))

(defc ordered-triples-6-12 (list-str (ordered-triples 6 12)))
