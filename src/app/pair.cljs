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

(defn list-str [xs]
  (let [go (fn [acc r]
             (if (nil? r)
               (str acc " )")
               (recur (str acc " "(car' r)) (cdr' r))))]
    (go "(" xs)))
