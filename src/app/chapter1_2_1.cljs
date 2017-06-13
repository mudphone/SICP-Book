(ns app.chapter1-2-1
  (:require
   [javelin.core :refer [defc]]))

;; Exercise 1.10
(defn ack [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (ack (- x 1)
                   (ack x (- y 1)))))

(defc a-1-10 (ack 1 10))
(defc a-2-4 (ack 2 4))
(defc a-3-3 (ack 3 3))

;; Exeercise: Counting change
(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(defn cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount
                     (dec kinds-of-coins))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins))))

(defn count-change [amount]
  (cc amount 5))

(defc cc-100 (count-change 100))

;; Exercise 1.11
(defn f1 [n]
  (cond (< n 3) n
        :else (+ (f1 (- n 1))
;32;31M                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3))))))

(defn f2-result [[n-1 n-2 n-3] count]
  (if (< count 3)
    count
    (+ n-1 (* 2 n-2) (* 3 n-3))))

(defn f2-iter [last-3 counter max-count]
  (let [result (f2-result last-3 counter)]
    (cond (= counter max-count) result 
          :else (f2-iter (take 3 (cons result last-3))
                         (inc counter)
                         max-count))))

(defn f2 [n]
  (f2-iter [0 0 0] 0 n))

(defc f1-0 (f1 0))
(defc f1-1 (f1 1))
(defc f1-2 (f1 2))
(defc f1-3 (f1 3))
(defc f1-4 (f1 4))
(defc f1-5 (f1 5))
(defc f1-6 (f1 6))
(defc f1-7 (f1 7))
(defc f1-8 (f1 8))
(defc f1-9 (f1 9))
(defc f1-10 (f1 10))

(defc f2-0 (f2 0))
(defc f2-1 (f2 1))
(defc f2-5 (f2 5))
(defc f2-6 (f2 6))
(defc f2-9 (f2 9))
(defc f2-10 (f2 10))

;; Exercise 1.12
(defn row
  ([n]
   (cond (= n 1) [1]
         (= n 2) [1 1]
         :else (row (row (dec n))
                    n)))
  ([last-row n]
   (vec
    (map-indexed #(if (or (= %1 0) (= %1 (dec n)))
                    1
                    (+ (nth last-row (dec %1))
                       (nth last-row %1)))
                 (repeat n 1)))))

;; Exercise 1.13
(def phi (/ (+ 1
               (Math/sqrt 5))
            2))

(def psi (/ (- 1
               (Math/sqrt 5))
            2))

(defn fib-nearest-phi [n]
  (/ (Math/pow phi n)
     (Math/sqrt 5)))

(defn fib-phi-psi [n]
  (/ (- (Math/pow phi n)
        (Math/pow psi n))
     (Math/sqrt 5)))

