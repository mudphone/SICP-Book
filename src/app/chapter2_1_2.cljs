(ns app.chapter2-1-2
  (:require
   [app.segment :refer [midpoint-segment make-segment]]
   [app.point :refer [make-point x-point y-point]]
   [javelin.core :refer [defc defc= cell=]]
   [hoplon.jquery]))

;; Exercise 2.2
(def canvas-id "ex-2-2-canvas")
(def canvas-size {:width 400 :height 400})

(def point1 (make-point 10 60))
(def point2 (make-point 40 140))

(defc midpoint-segment-1-2
  (-> (midpoint-segment
       (make-segment point1 point2))))

(defn draw-point [ctx color radius x y]
  (aset ctx "fillStyle" color)
  (.beginPath ctx)
  (.arc ctx x y radius 0 (* 2 Math/PI))
  (.closePath ctx)
  (.fill ctx)
  ctx)

(defn draw-ex-2-2 []
  (let [canvas (.getElementById js/document canvas-id)
        ctx (.getContext canvas "2d")]
    (-> ctx
        (draw-point "red" 2
                    (x-point @midpoint-segment-1-2)
                    (y-point @midpoint-segment-1-2))
        (draw-point "black" 4
                    (x-point point1)
                    (y-point point1))
        (draw-point "black" 4
                    (x-point point2)
                    (y-point point2)))))
