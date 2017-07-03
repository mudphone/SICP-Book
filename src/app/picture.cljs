(ns app.picture)

;; FRAME
(defrecord Frame [origin edge1 edge2])

(defn make-frame [origin edge1 edge2]
  (map->Frame {:origin origin :edge1 edge1 :edge2 edge2}))

(defn origin-frame [frame]
  (:origin frame))

(defn edge1-frame [frame]
  (:edge1 frame))

(defn edge2-frame [frame]
  (:edge2 frame))

;; VECTOR
(defn make-vect [x y] [x y])
(defn xcor-vect [[x _]] x)
(defn ycor-vect [[_ y]] y)
(defn vect-str [v]
  (str "[ " (xcor-vect v) " , " (ycor-vect v) " ]"))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [k v]
  (make-vect (* k (xcor-vect v))
             (* k (ycor-vect v))))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

