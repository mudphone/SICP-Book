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

;; SEGMENT
(defn make-segment [v1 v2]
  (make-vect v1 v2))

(defn start-segment [segment]
  (xcor-vect segment))

(defn end-segment [segment]
  (ycor-vect segment))

;; LINE
(def CANVAS-HEIGHT 400)
(def CANVAS-WIDTH 400)

(defn canvas [id]
  (.getElementById js/document id))

(def canvas-context (atom nil))

(defn get-canvas-context [canvas]
  (.getContext canvas "2d"))

(defn set-canvas-context [id]
  (->> (canvas id)
       (get-canvas-context)
       (reset! canvas-context)))

(def line-color (atom "black"))

(defn draw-line
  ([p1 p2]
   (draw-line @canvas-context @line-color p1 p2))
  ([ctx color p1 p2]
   (let [x1 (xcor-vect p1)
         y1 (ycor-vect p1)
         x2 (xcor-vect p2)
         y2 (ycor-vect p2)]
     (aset ctx "strokeStyle" color)
     (.beginPath ctx)
     (.moveTo ctx x1 y1)
     (.lineTo ctx x2 y2)
     (.stroke ctx)
     ctx)))

;; PAINTING
(defn segments->painter [segment-list]
  (fn [frame]
    (doseq [segment segment-list]
     (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))))
