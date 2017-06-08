(ns app.rectangle)

(defrecord Rectangle [origin width height])

(defn make-rect [origin width height]
  (map->Rectangle {:origin origin :width width :height height}))

(defn origin [rect]
  (:origin rect))

(defn width [rect]
  (:width rect))

(defn height [rect]
  (:height rect))
