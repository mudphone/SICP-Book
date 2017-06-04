(ns app.rational)

(defrecord Rational [numerator denominator])

(defn abs [x] (if (< x 0) (- x) x))

(defn make-rat [n d]
  (let [[n d] (cond
                (and (neg? n) (neg? d)) [   (abs n)  (abs d)]
                (or  (neg? n) (neg? d)) [(- (abs n)) (abs d)]
                :else                   [        n        d])]
    (map->Rational {:numerator n :denominator d})))

(defn numer [rat]
  (:numerator rat))

(defn denom [rat]
  (:denominator rat))

(defn rat-str [rat]
  (str (numer rat) "/" (denom rat)))

