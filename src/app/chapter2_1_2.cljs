(ns app.chapter2-1-2
  (:require
   [javelin.core :refer [defc defc= cell=]]
   [hoplon.jquery]))

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
