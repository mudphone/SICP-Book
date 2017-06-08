(ns app.segment
  (:require [app.point :refer [x-point y-point make-point]]))

(defrecord Segment [start end])

(defn make-segment [start end]
  (map->Segment {:start start :end end}))

(defn start-segment [segment]
  (:start segment))

(defn end-segment [segment]
  (:end segment))

(defn avg [a b]
  (/ (+ a b) 2.0))

(defn midpoint-segment [segment]
  (let [start (start-segment segment)
        x-start (x-point start)
        y-start (y-point start)
        end (end-segment segment)
        x-end (x-point end)
        y-end (y-point end)]
    (make-point (avg x-start x-end)
                (avg y-start y-end))))
