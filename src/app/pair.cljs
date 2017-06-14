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

(defn list-str [xs]
  (let [go (fn [acc r]
             (if (null?' r)
               (str acc " )")
               (recur (str acc " "(car' r)) (cdr' r))))]
    (go "(" xs)))

(defn map' [proc items]
  (if (null?' items)
    nil
    (cons' (proc (car' items))
           (map' proc (cdr' items)))))
