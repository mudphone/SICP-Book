(ns app.chapter1-3-1
  (:require [javelin.core :refer [defc]]))

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
