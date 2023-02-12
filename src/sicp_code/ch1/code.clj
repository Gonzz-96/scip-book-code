(ns sicp-code.ch1.code
  (:gen-class))

(defn square [x] (* x x))
;; (square 21)
;; (square (+ 2 5))
;; (square (square 3))

(defn sum-of-squares
  [x y]
  (+ (square x) (square y)))
;; (sum-of-squares 3 4)

(defn f [a] (sum-of-squares (+ a 1) (* a 2)))
;; (f 5)

;; case analysis
(defn my-abs [x]
  (cond (> x 0) x
        (= x 0) 0
        (< x 0) (- x)))

;; (my-abs 10)
;; (my-abs -10)

(defn my-abs-2 [x]
  (cond (< x 0) (- x)
        :else x))

;; (my-abs-2 10)
;; (my-abs-2 -10)

(defn my-abs-3 [x]
  (if (< x 0)
    (- x)
    x))

;; (my-abs-3 10)
;; (my-abs-3 -10)

;; Example: square roots by Newton's method

(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve guess x)
           x)))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn good-enough?
  [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; (sqrt 9)
;; (sqrt (+ 100 37)
;; (square (sqrt 1000)


