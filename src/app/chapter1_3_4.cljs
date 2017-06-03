(ns app.chapter1-3-4
  (:require
   [javelin.core :refer [defc defc= cell=]]
   [app.chapter1-3-3 :refer [fixed-point2 abs tolerance]]))

;; Exercise 1.40
(defn cube   [x] (* x x x))
(defn square [x] (* x x))

(defn cubic [a b c]
  (fn [x]
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(def dx 0.00001)

(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point2 (newton-transform g) guess))

(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x))
                  1.0))

(defc newtons-method-cubic-1-2-3 (newtons-method (cubic 1 2 3) 1.0))

;; Exercise 1.41
(defn dbl [f]
  (fn [x]
    (f (f x))))

(defn inc-16 [x]
  (((dbl (dbl dbl)) inc) x))

(defc inc-16-5 (inc-16 5))

;; Exercise 1.42
(defn compose [f g]
  (fn [x]
    (f (g x))))

(defc compose-square-inc-6 ((compose square inc) 6))

;; Exercise 1.43
(defn repeated [f n]
  (let [it (fn [n result]
             (if (< n 1)
               result
               (recur (dec n) (compose f result))))]
    (it n identity)))

(defn repeated-recur [f n]
  (if (< n 1)
    (fn [x] x)
    (compose f (repeated-recur f (dec n)))))

(defc repeated-square-2-5 ((repeated square 2) 5))

;; Exercise 1.44
(defn smooth [f]
  (fn [x]
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3.0)))

(defn n-fold-smooth [f n]
  ((repeated smooth n) f))

;; Exercise 1.45
; Credit to the Scheme Wiki: http://community.schemewiki.org/?sicp-ex-1.45
(defn average [x y] 
   (/ (+ x y) 2.0)) 
  
(defn average-damp [f] 
  (fn [x] (average x (f x))))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn nth-root [n x]
  (fixed-point2 ((repeated average-damp (Math/floor (log2 n)))
                 (fn [y] (/ x (Math/pow y (dec n)))))
                1.0))

(defc nth-root-3-1000  (nth-root 3 1000))
(defc nth-root-4-10000 (nth-root 4 10000))
(defc nth-root-5-100000 (nth-root 5 100000))

;; Exercise 1.46
(defn iterative-improve [good-enuf? improve]
  (fn [guess]
    (let [new-guess (improve guess)]
      (if (good-enuf? guess new-guess)
        new-guess
        ((iterative-improve good-enuf? improve) new-guess)))))

(defn close-enough? [x y]
  (< (/ (abs (- x y)) y) tolerance))

(defn sqrt-iter-improve [x]
  ((iterative-improve close-enough?
                      (average-damp (fn [y] (/ x y))))
   1.0))

(defn fixed-point [f guess] 
   ((iterative-improve
     f 
     close-enough?) guess)) 

(defc sqrt-iter-improve-9 (sqrt-iter-improve 9))
