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
(defn pi-n [n prod]
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

(defn prod-iterative [term a next b]
  (let [go (fn [a result]
             (if (> a b)
               result
               (recur (next a) (* (term a) result))))]
    (go a 1)))

(defn pi-prod [n]
  (pi-n n prod-iterative))

(defc pi-prod-1000 (pi-prod 1000))

(defn prod-recursive [term a next b]
  (if (> a b)
    1
    (* (term a) (prod-recursive term (next a) next b))))

(defn pi-prod-recursive [n]
  (pi-n n prod-recursive))

(defc pi-prod-recursive-1000 (pi-prod-recursive 1000))

;; Exercise 1.32: Iterative
(defn accumulate [combiner null-value term a next b]
  (let [go (fn [a acc]
             (if (> a b)
               acc
               (recur (next a) (combiner (term a) acc))))]
    (go a null-value)))

(defn acc-sum [term a next b]
  (accumulate + 0 term a next b))

(defn acc-sum-cubes [a b]
  (acc-sum cube a inc b))

(defn acc-sum-simple [a b]
  (acc-sum identity a inc b))

(defc acc-sum-cubes-0-10 (acc-sum-cubes 0 10))
(defc acc-sum-simple-0-5 (acc-sum-simple 0 5))

(defn acc-product [term a next b]
  (accumulate * 1 term a next b))

(defn acc-product-cubes [a b]
  (acc-product cube a inc b))

(defn acc-product-simple [a b]
  (acc-product identity a inc b))

(defc acc-product-cubes-2-3 (acc-product-cubes 2 3))
(defc acc-product-simple-5-6 (acc-product-simple 5 6))

;; Exercise 1.32: Recursive
(defn accumulate-recursive [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-recursive combiner null-value term (next a) next b))))

(defn acc-sum-recursive [term a next b]
  (accumulate-recursive + 0 term a next b))

(defn acc-sum-recursive-cubes [a b]
  (acc-sum-recursive cube a inc b))

(defc acc-sum-recursive-cubes-0-10 (acc-sum-recursive-cubes 0 10))
