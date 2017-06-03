(ns app.chapter1-3-3
  (:require
   [javelin.core :refer [defc defc= cell=]]))

;; Exercise 1.35
(def tolerance 0.00001)

(defn abs [x] (if (< x 0) (- x) x))

(defn fixed-point [f first-guess]
  (let [close-enough? (fn [v1 v2]
                        (< (abs (- v1 v2)) tolerance))
        try-it (fn [{:keys [steps guess]}]
                 (let [next (f guess)]
                   (println "Ex 1.35 next guess: " next)
                   (if (close-enough? guess next)
                     {:steps steps :guess next}
                     (recur {:steps (inc steps) :guess next}))))]
    (println "Ex 1.35 starting guess: " first-guess)
    (try-it {:steps 1 :guess first-guess})))

(defn golden-ratio []
  (fixed-point (fn [y]
                 (+ 1.0 (/ 1.0 y)))
               1.0))

(defc the-golden-ratio (golden-ratio))
(defc= golden-ratio-value (:guess the-golden-ratio))
(defc= golden-ratio-steps (:steps the-golden-ratio))

;; Exercise 1.36
;; x   log(1000)/log(x)
(defn x-to-x-1000 []
  (fixed-point (fn [y]
                 (/ (.log js/Math 1000) (.log js/Math y)))
               2.0))

(defc the-x-to-x-1000 (x-to-x-1000))
(defc= x-to-x-1000-value (:guess the-x-to-x-1000))
(defc= x-to-x-1000-steps (:steps the-x-to-x-1000))

(defn x-to-x-1000-avg-damp []
  (fixed-point (fn [y]
                 (/ (+ y (/ (.log js/Math 1000) (.log js/Math y))) 2.0))
               2.0))

(defc the-x-to-x-1000-avg-damp (x-to-x-1000-avg-damp))
(defc= x-to-x-1000-avg-damp-value (:guess the-x-to-x-1000-avg-damp))
(defc= x-to-x-1000-avg-damp-steps (:steps the-x-to-x-1000-avg-damp))
