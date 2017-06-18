(ns app.pair)

(defrecord Pair [car cdr])

(defn cons' [x y]
  (map->Pair {:car x :cdr y}))

(defn car' [p] (:car p))
(defn cdr' [p] (:cdr p))

(defn list' [& xs]
  (if (seq xs)
    (cons' (first xs) (apply list' (rest xs)))
    nil))

(defn null?' [xs]
  (nil? xs))

(defn pair? [x]
  (= Pair (type x)))

(defn list-str [xs]
  (let [go (fn [acc r]
             (cond
               (null?' r) (str acc " )")
               (not (pair? r)) r
               :else (recur (str acc " " (list-str (car' r))) (cdr' r))))]
    (go "(" xs)))

(defn reverse' [xs]
  (let [go (fn [acc r]
             (if (nil? r)
               acc
               (recur (cons' (car' r) acc) (cdr' r))))]
    (go nil xs)))

(defn deep-reverse [xs]
  (let [go (fn [acc r]
             (if (nil? r)
               acc
               (let [c (car' r)
                     x (if (not (pair? c))
                         c
                         (deep-reverse c))]
                 (recur (cons' x acc) (cdr' r)))))]
    (go nil xs)))

(defn map' [proc items]
  (if (null?' items)
    nil
    (cons' (proc (car' items))
           (map' proc (cdr' items)))))

(defn for-each [proc items]
  (if (null?' items)
    true
    (do
      (proc (car' items))
      (recur proc (cdr' items)))))

(defn length [items]
  (if (null?' items)
      0
      (inc (length (cdr' items)))))

(defn list-ref [items n]
  (if (= n 0)
      (car' items)
      (recur (cdr' items) (dec n))))

(defn count-leaves [x]
  (cond
    (null?' x) 0
    (not (pair? x)) 1
    :else (+ (count-leaves (car' x))
             (count-leaves (cdr' x)))))

(defn append [list1 list2]
  (if (null?' list1)
      list2
      (cons' (car' list1)
             (append (cdr' list1) list2))))

(defn tree-map [proc tree]
    (map' (fn [sub-tree]
            (if (pair? sub-tree)
              (tree-map proc sub-tree)
              (proc sub-tree)))
        tree))
