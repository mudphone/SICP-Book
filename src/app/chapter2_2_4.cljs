(ns app.chapter2-2-4
  (:require
   [app.pair :as pair]
   [app.picture :as p :refer [beside make-frame make-segment make-vect
                              segments->painter set-canvas-context]]
   [javelin.core :refer [defc]]))

(declare below)
(declare flip-vert)
(declare flip-horiz)
(declare corner-split)
(declare rotate180)

(defn flipped-pairs [painter]
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

;; Exercise 2.44
(defn right-split [painter n]
  (if (= n 0)
      painter
      (let [smaller (right-split painter (dec n))]
        (beside painter (below smaller smaller)))))

(defn up-split [painter n]
  (if (= n 0)
      painter
      (let [smaller (up-split painter (dec n))]
        (below painter (beside smaller smaller)))))

(defn corner-split [painter n]
  (if (= n 0)
      painter
      (let [up    (up-split painter (dec n))
            right (right-split painter (dec n))
            top-left     (beside up up)
            bottom-right (below right right)
            corner       (corner-split painter (dec n))]
        (beside (below painter top-left)
                (below bottom-right corner)))))

(defn square-of-four [tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below bottom top))))

(defn flipped-pairs2 [painter]
  (let [combine4 (square-of-four identity flip-vert
                                 identity flip-vert)]
    (combine4 painter)))

(defn square-limit2 [painter n]
  (let [combine4 (square-of-four flip-horiz identity
                                 rotate180 flip-vert)]
    (combine4 (corner-split painter n))))

;; Exercise 2.45
(defn split [op1 op2]
  (letfn [(combo [painter n]
            (if (= n 0)
              painter
              (let [smaller (combo painter (dec n))]
                (op1 painter (op2 smaller smaller)))))]
    combo))

(def right-split2 (split beside below))
(def up-split2    (split below beside))

;; Exercise 2.46
(defc add-v12-v34 (p/vect-str
                   (p/add-vect (p/make-vect 1 2) (p/make-vect 3 4))))
(defc sub-v12-v34 (p/vect-str
                   (p/sub-vect (p/make-vect 1 2) (p/make-vect 3 4))))
(defc scale-10-v12 (p/vect-str
                    (p/scale-vect 10 (p/make-vect 1 2))))

;; Exercise 2.47
(defn make-frame-list [origin edge1 edge2]
  (pair/list' origin edge1 edge2))

(defn origin-frame-list [frame]
  (pair/car' frame))

(defn edge1-frame-list [frame]
  (pair/cadr' frame))

(defn edge2-frame-list [frame]
  (pair/caddr' frame))

(def frame-list (make-frame-list [0 0] [1 0] [1 1]))
(defc origin-list-00-10-11 (str
                            (origin-frame-list frame-list)))
(defc edge1-list-00-10-11 (str
                           (edge1-frame-list frame-list)))
(defc edge2-list-00-10-11 (str
                           (edge2-frame-list frame-list)))

(defn make-frame-cons [origin edge1 edge2]
  (pair/cons' origin (pair/cons' edge1 edge2)))

(def origin-frame-cons origin-frame-list)
(def edge1-frame-cons edge1-frame-list)

(defn edge2-frame-cons [frame]
  (pair/cddr' frame))

(def frame-cons (make-frame-cons [0 0] [1 0] [1 1]))
(defc origin-cons-00-10-11 (str
                            (origin-frame-cons frame-cons)))
(defc edge1-cons-00-10-11 (str
                           (edge1-frame-cons frame-cons)))
(defc edge2-cons-00-10-11 (str
                           (edge2-frame-cons frame-cons)))

;; Exercise 2.48 see app.picture

;; Exercise 2.49
;; a.  The painter that draws the outline of the designated frame.
(defn outline [frame]
  (let [painter (segments->painter
                 [(make-segment (make-vect 0 0) (make-vect 0 1))
                  (make-segment (make-vect 0 1) (make-vect 1 1))
                  (make-segment (make-vect 1 1) (make-vect 1 0))
                  (make-segment (make-vect 1 0) (make-vect 0 0))])]
    (painter frame)))

(defn draw-2-49a [id]
  (set-canvas-context id)
  (outline (make-frame (make-vect 0 0)
                       (make-vect 0 40)
                       (make-vect 100 0))))

;; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
(defn draw-x [frame]
  (let [painter (segments->painter
                 [(make-segment (make-vect 0 0) (make-vect 1 1))
                  (make-segment (make-vect 0 1) (make-vect 1 0))])]
    (painter frame)))

(defn draw-2-49b [id]
  (set-canvas-context id)
  (draw-x (make-frame (make-vect 0 0)
                      (make-vect 0 40)
                      (make-vect 100 0))))

;; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(defn diamond [frame]
  (let [painter (segments->painter
                 [(make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                  (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                  (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                  (make-segment (make-vect 1 0.5) (make-vect 0.5 0))])]
    (painter frame)))

(defn draw-2-49c [id]
  (set-canvas-context id)
  (diamond (make-frame (make-vect 0 0)
                       (make-vect 0 40)
                       (make-vect 100 0))))

;; d.  The wave painter.
(def wave-segments
  [(make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))
   (make-segment (make-vect 0.006 0.635) (make-vect 0.155 0.392))
   (make-segment (make-vect 0.304 0.646) (make-vect 0.155 0.591))
   (make-segment (make-vect 0.298 0.591) (make-vect 0.155 0.392))
   (make-segment (make-vect 0.304 0.646) (make-vect 0.403 0.646))
   (make-segment (make-vect 0.298 0.591) (make-vect 0.354 0.492))
   (make-segment (make-vect 0.403 0.646) (make-vect 0.348 0.845))
   (make-segment (make-vect 0.354 0.492) (make-vect 0.249 0.000))
   (make-segment (make-vect 0.403 0.000) (make-vect 0.502 0.293))
   (make-segment (make-vect 0.502 0.293) (make-vect 0.602 0.000))
   (make-segment (make-vect 0.348 0.845) (make-vect 0.403 0.999))
   (make-segment (make-vect 0.602 0.999) (make-vect 0.652 0.845))
   (make-segment (make-vect 0.652 0.845) (make-vect 0.602 0.646))
   (make-segment (make-vect 0.602 0.646) (make-vect 0.751 0.646))
   (make-segment (make-vect 0.751 0.646) (make-vect 0.999 0.343))
   (make-segment (make-vect 0.751 0.000) (make-vect 0.597 0.442))
   (make-segment (make-vect 0.597 0.442) (make-vect 0.999 0.144))])

(defn wave [frame]
  ((p/rotate180 (segments->painter wave-segments)) frame))
#_(defn wave [frame]
  ((segments->painter wave-segments) frame))


(defn draw-2-49d [id]
  (set-canvas-context id)
  (wave (make-frame (make-vect 0 0)
                    (make-vect 0 40)
                    (make-vect 100 0))))

;; Exercise 2.50
(defn draw-p [frame]
  ((segments->painter [(make-segment (make-vect 0.1 0.1) (make-vect 0.9 0.1))
                       (make-segment (make-vect 0.9 0.1) (make-vect 0.9 0.5))
                       (make-segment (make-vect 0.9 0.5) (make-vect 0.1 0.5))
                       (make-segment (make-vect 0.1 0.1) (make-vect 0.1 0.9))]) frame))

(defn draw-2-50-p [id]
  (set-canvas-context id)
  (draw-p (make-frame (make-vect 0 0)
                      (make-vect 0 40)
                      (make-vect 100 0))))

(defn draw-2-50-rotate90 [id]
  (set-canvas-context id)
  ((p/rotate90 wave)
   (make-frame (make-vect 0 0)
               (make-vect 0 40)
               (make-vect 100 0))))

(defn draw-2-50-beside [id]
  (set-canvas-context id)
  ((beside wave wave)
   (make-frame (make-vect 0 0)
                    (make-vect 0 40)
                    (make-vect 100 0))))
