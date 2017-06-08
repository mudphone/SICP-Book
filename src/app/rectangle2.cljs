(ns app.rectangle2)

(defn make-rect [o w h]
  [o w h])

(defn origin [rect]
  (first rect))

(defn width [rect]
  (nth rect 1))

(defn height [rect]
  (last rect))
