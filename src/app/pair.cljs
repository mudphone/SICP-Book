(ns app.pair)

(defrecord Pair [car cdr])

(defn cons' [x y]
  (map->Pair {:car x :cdr y}))

(defn car' [p] (:car p))
(defn cdr' [p] (:cdr p))

(defn cadr' [p] (car' (cdr' p)))
(defn cddr' [p] (cdr' (cdr' p)))
(defn caddr' [p] (car' (cddr' p)))

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

(declare accumulate)
(defn map-n [proc & seqs]
  (if (null?' (car' (first seqs)))
    nil
    (cons' (apply proc (map car' seqs))
           (apply (partial map-n proc) (map cdr' seqs)))))

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

(defn list-n [items ref]
  (let [go (fn [n]
             (cond
               (>= n (length items)) nil
               (= (list-ref items n) ref) n
               :else (recur (inc n))))]
    (go 0)))

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

(defn filter' [predicate sequence]
  (cond (null?' sequence) nil
        (predicate (car' sequence)) (cons' (car' sequence)
                                           (filter' predicate (cdr' sequence)))
        :else (filter' predicate (cdr' sequence))))

(defn accumulate [op initial sequence]
  (if (null?' sequence)
      initial
      (op (car' sequence)
          (accumulate op initial (cdr' sequence)))))

(defn enumerate-interval [low high]
  (if (> low high)
      nil
      (cons' low (enumerate-interval (+ low 1) high))))

(defn enumerate-tree [tree]
  (cond (null?' tree) nil
        (not (pair? tree)) (list' tree)
        :else (append (enumerate-tree (car' tree))
                      (enumerate-tree (cdr' tree)))))

(defn accumulate-n [op init seqs]
  (if (null?' (car' seqs))
      nil
      (cons' (accumulate op init (map' car' seqs))
             (accumulate-n op init (map' cdr' seqs)))))

(def fold-right accumulate)

(defn fold-left [op initial sequence]
  (let [it (fn [result r]
             (if (null?' r)
               result
               (recur (op result (car' r))
                      (cdr' r))))]
    (it initial sequence)))

(defn flatmap [proc seq]
  (accumulate append nil (map' proc seq)))

(defn remove' [item sequence]
  (filter' (fn [x]
             (not (= x item)))
           sequence))
