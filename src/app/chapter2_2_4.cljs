(ns app.chapter2-2-4
  (:require
   [app.picture :as p]
   [javelin.core :refer [defc]]))

(declare beside)
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
