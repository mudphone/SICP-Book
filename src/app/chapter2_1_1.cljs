(ns app.chapter2-1-1
  (:require
   [app.rational :refer [make-rat numer denom rat-str]]
   [javelin.core :refer [defc defc= cell=]]
   [hoplon.jquery]))

;; Exercise 2.1
(defc make-rat-1-2 (rat-str (make-rat 1 2)))
(defc numer-rat-neg1-neg2 (numer (make-rat -1 -2)))
(defc denom-rat-neg1-neg2 (denom (make-rat -1 -2)))
(defc numer-rat-1-neg2 (numer (make-rat 1 -2)))
(defc denom-rat-1-neg2 (denom (make-rat 1 -2)))

;; Exercise 2.2
(def canvas-id "ex-2-2-canvas")
(def canvas-size {:width 400 :height 400})

(defn draw-ex-2-2 []
  (let [canvas (.getElementById js/document canvas-id)
        ctx (.getContext canvas "2d")]
    (aset ctx "fillStyle" "black")
    (.beginPath ctx)
    (.arc ctx 100 50 2 0 (* 2 Math/PI))
    (.closePath ctx)
    (.fill ctx)))
