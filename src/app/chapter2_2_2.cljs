(ns app.chapter2-2-2
  (:require
   [javelin.core :refer [defc]]
   [app.pair :refer [append car' cdr' cons' deep-reverse
                     list' list-str map' null?' pair? reverse'
                     tree-map]]))

(defn sq [x] (* x x))

;; Exercise 2.25
(defc seven1 (car' (cdr' (car' (cdr' (cdr' (list' 1 3 (list' 5 7) 9)))))))
(defc seven2 (car' (car' (list' (list' 7)))))
(defc seven3 (let [x (list' 1 (list' 2 (list' 3 (list' 4 (list' 5 (list' 6 7))))))]
               (car'
                (cdr'
                 (car'
                  (cdr'
                   (car'
                    (cdr'
                     (car'
                      (cdr'
                       (car'
                        (cdr'
                         (car'
                          (cdr' x))))))))))))))

;; Exercise 2.26
(def list-x (list' 1 2 3))
(def list-y (list' 4 5 6))
(defc append-lists (list-str (append list-x list-y)))
(defc cons-lists (list-str (cons' list-x list-y)))
(defc list-lists (list-str (list' list-x list-y)))

;; Exercise 2.27
(def tree-x (list' (list' 1 2) (list' 3 4)))
(defc deep-reverse-12-34 (list-str (deep-reverse tree-x)))

;; Exercise 2.28
(defn fringe [x]
  (cond
    (null?' x) nil
    (not (pair? x)) x

    (pair? (car' x))
    (fringe (append (car' x) (cdr' x)))
    
    :else (cons' (car' x)
                 (fringe (cdr' x)))))

(defc fringe-1-6 (list-str (fringe (list' (list' 1 (list' 2 3) 4) (list' 5 6)))))

;; Exercise 2.29
;; (defn make-mobile [left right]
;;   (list' left right))

;; (defn make-branch [length structure]
;;   (list' length structure))

;; (defn is-mobile? [m]
;;   (pair? m))

(defn make-mobile [left right]
  [left right])

(defn make-branch [length structure]
  [length structure])

(defn is-mobile? [m]
  (and (not (int? m))
       (seq m)))

;; a)
(defmulti left-branch (fn [mobile] (type mobile)))
(defmethod left-branch PersistentVector
  [mobile]
  (first mobile))
(defmethod left-branch app.pair/Pair
  [mobile]
  (car' mobile))

(defmulti right-branch (fn [mobile] (type mobile)))
(defmethod right-branch PersistentVector
  [mobile]
  (last mobile))
(defmethod right-branch app.pair/Pair
  [mobile]
  (car' (cdr' mobile)))

(defmulti branch-length (fn [branch] (type branch)))
(defmethod branch-length PersistentVector
  [branch]
  (first branch))
(defmethod branch-length app.pair/Pair
  [branch]
  (car' branch))

(defmulti branch-structure (fn [branch] (type branch)))
(defmethod branch-structure PersistentVector
  [branch]
  (last branch))
(defmethod branch-structure app.pair/Pair
  [branch]
  (car' (cdr' branch)))

;; b)
(declare total-weight)
(defn branch-weight [branch]
  (let [s (branch-structure branch)]
    (if (not (is-mobile? s))
      s
      (total-weight s) )))

(defn total-weight [mobile]
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(def mobile (make-mobile
             (make-branch 1 (make-mobile
                             (make-branch 2 20)
                             (make-branch 3 30)))
             (make-branch 4 (make-mobile
                             (make-branch 5 50)
                             (make-branch 6 (make-mobile
                                             (make-branch 7 70)
                                             (make-branch 8 80)))))))

(defc total-weight-mobile (total-weight mobile))

;; c)
(defn torque [branch]
  (* (branch-length branch) (branch-weight branch)))

(defn balanced [mobile]
  (let [go (fn [m acc]
             (cond
               (false? acc) false
               :else
               (let [left-br (left-branch m)
                     right-br (right-branch m)
                     left-br-has-mobile? (is-mobile? (branch-structure left-br))
                     right-br-has-mobile? (is-mobile? (branch-structure right-br))]
                 (and
                  (= (torque left-br) (torque right-br))
                  (if left-br-has-mobile?
                    (balanced (branch-structure left-br))
                    true)
                  (if right-br-has-mobile?
                    (balanced (branch-structure right-br))
                    true)))))]
    (go mobile true)))

(def balanced-mobile (make-mobile (make-branch 1 4) (make-branch 1 (make-mobile (make-branch 2 2) (make-branch 2 2)))))

(defc balanced-mobile-14-22-22 (str (balanced balanced-mobile)))

;; Exercise 2.30
(defn square-tree [tree]
  (cond (null?' tree) nil
        (not (pair? tree)) (sq tree)
        :else (cons' (square-tree (car' tree))
                     (square-tree (cdr' tree)))))

(def tree (list' 1 (list' 2 (list' 3 4) 5) (list' 6 7)))

(defc square-a-tree (list-str (square-tree tree)))

(defn square-tree-map [tree]
  (map' (fn [sub-tree]
          (if (pair? sub-tree)
            (square-tree-map sub-tree)
            (sq sub-tree)))
        tree))

(defc square-map-a-tree (list-str (square-tree-map tree)))

;; Exercise 2.31
(defn square-tree-map' [tree]
  (tree-map sq tree))

(defc square-map2-a-tree (list-str (square-tree-map' tree)))

;; Exercise 2.32
(defn subsets [s]
  (if (null?' s)
      (list' nil)
      (let [r (subsets (cdr' s))]
        (append r (map' (fn [x] (cons' (car' s) x))
                        r)))))

(defc subsets-123 (list-str (subsets (list' 1 2 3))))
