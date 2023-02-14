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

;; 1.2.2 tree recursion
(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2))))) ;; => not tail-recursive

(defn iterative-fib [n]
  (defn fib-iter
    [a b count]
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; Example: counting change

(defn count-change [amount]
  (cc amount 5))

(defn cc
  [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins))))

(defn first-denomination
  [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

;; (count-change 100)

