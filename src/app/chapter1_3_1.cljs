(ns app.chapter1-3-1
  (:require [javelin.core :refer [defc cell=]]))

;; Exercise 1.29
(defn simpsons [f a b n]
  (let [h (/ (- b a) n)
        y (fn [k]
            (f (+ a (* k h))))
        rng (range (inc n))
        coef (fn [i]
               (cond
                 (or (= i 0) (= i (inc n))) 1
                 (odd? i) 4
                 :else 2))
        sum-ys (reduce +
                       (map (fn [i]
                              (* (coef i) (y i)))
                            rng))]
    (* (/ h 3) sum-ys)))

(defn cube [x] (* x x x))

(defc simpsons-cube-0-1-100 (simpsons cube 0 1 100))
(defc simpsons-cube-0-1-1000 (simpsons cube 0 1 1000))

;; Exercise 1.30
(defn sum-linear-recursion [term a next b]
  (if (> a b)
      0
      (+ (term a)
         (sum-linear-recursion term (next a) next b))))

(defn sum-cubes [a b]
  (sum-linear-recursion cube a inc b))

(defn sum-iter [term a next b]
  (let [iterr (fn [a result]
               (if (> a b)
                   result
                   (recur (next a)
                          (+ (term a) result))))]
    (iterr a 0)))

(defc iterative-sum-cubes-1-10 (sum-iter cube 1 inc 10))

;; Exercise 1.31
(defn prod [term a next b]
  (let [iterr (fn [a result]
                (if (> a b)
                  result
                  (recur (next a) (* (term a) result))))]
    (iterr a 1)))

(defn pi-prod [n]
  (let [evens-from-2 (apply concat (map (fn [x] [x x]) (iterate #(+ 2 %) 2)))
        next-numerator (fn [n]
                         (nth (drop 1 evens-from-2) n))
        odds-from-3 (apply concat (map (fn [x] [x x]) (iterate #(+ 2 %) 3)))
        next-denominator (fn [n]
                           (nth odds-from-3 n))
        term (fn [n]
               (/ (next-numerator n)
                  (next-denominator n)))]
    (* 4 (prod term 0 inc n))))

(defc pi-prod-1000 (pi-prod 1000))
