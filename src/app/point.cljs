(ns app.point)

(defrecord Point [x y])

(defn make-point [x y]
  (map->Point {:x x :y y}))

(defn x-point [point]
  (:x point))

(defn y-point [point]
  (:y point))

(defn point-str [p]
  (str "(" (x-point p) "," (y-point p) ")"))

(defn print-point [p]
  (-> (point-str p)
      (println)))
