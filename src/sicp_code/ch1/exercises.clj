(ns sicp-code.ch1.exercises
  (:require [sicp-code.ch1.code :as code])
  (:gen-class))

;; Exercise 1.1
;; 10 ;; => 10 
;; (+ 5 3 4) ;; => 12
;; (- 9 1) ;; => 8
;; (/ 6 2) ;; => 3
;; (+ (* 2 4) (- 4 6)) ;; => 6

(def a 3) ;; => a = 3
(def b (+ a 1)) ;; => b = 4

;; (+ a b (* a b)) ;; => 19
;; (= a b) ;; => false

;; (if (and (> b a ) (< b (* a b)))
;;   b
;;   a) ;; => b (4)

;; (cond (= a 4) 6
;;       (= b 4) (+ 6 7 a)
;;       :else 25) ;; => 16

;; (+ 2 (if (> b a) b a)) ;; => 6

;; (* (cond (> a b) a
;;          (< a b) b
;;          :else -1)
;;    (+ a 1)) ;; => 16

;; Exercise 1.2
;; (/ (+ 5 4 (- 2
;;              (- 3 (+ 6 (/ 4 5)))))
;;    (* 3
;;       (- 6 2)
;;       (- 2 7))) ;; => -37/150

;; Exercise 1.3: define a procedure that takes three numbers as arguments and returns the sum
;; of the squares of the two larger number
(defn square [x] (* x x))

(defn sum-of-squares
  [x y]
  (+ (square x) (square y)))

(defn sum-of-greater
  [a b c]
  (cond (and (>= a c) (>= b c)) (sum-of-squares a b)
        (and (>= b a) (>= c a)) (sum-of-squares b c)
        (and (>= a b) (>= c b)) (sum-of-squares a c)))

;; (sum-of-greater 10 9 8)
;; (sum-of-greater 2 2 2)
;; (sum-of-greater -10 30 2)

;; Exercise 1.4
;; Explanation: this method is using a conditional if statement to decide what compound
;; procedure (in this case, a built-in operator) is going to be applied to its operand.
;; From a semantic stand point, it would be like getting the abs value of b
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; (a-plus-abs-b 10 10)
;; (a-plus-abs-b 10 -10)
;; (a-plus-abs-b -10 -10)

;; Exercise: 1.5

(defn p [] (p))

(defn my-test 
  [x y]
  (if (= x 0)
    0
    y))

;; (test 0 (p))

;; Exercise 1.6: new if
(defn new-if
  [predicate then else]
  (cond predicate then
        :else else))

;; (new-if (= 2 3) 0 5)
;; (new-if (= 1 1) 0 5)

;; problematic call
(defn sqrt-iter
  [guess x]
  (new-if (code/good-enough? guess x)
          guess
          (sqrt-iter (code/improve guess x)
                     x)))

;; (sqrt-iter 1 9)

;; Explanation: while the special form "if" evaluates their then/else clases
;; depending on the value of the predicate passed to it, the "new-if" is a
;; new compound procedure, which means that it needs to evaluate its arguments
;; before being able to execute its actual instructions. Using "new-if" in a
;; recursive approach will generate an infinite loop, since the interpreter
;; will evaluate the function over and over 'til the stack gets overflowed.

;; Exercise 1.7; define a new sqrt-iter procedure to use the delta of our guess
;; as criteria of our predicate to stop recursion
(defn average [x y]
    (/ (+ x y) 2.0))

(defn good-enough? [guess prev-guess]
  (< (abs (- guess prev-guess)) 0.000001))

(defn improved-sqrt [x]
  (defn improve [guess x]
    (average guess (/ x guess)))
  (defn sqrt-iter
    [guess x prev-guess]
      (if (good-enough? guess prev-guess)
        guess
        (recur (improve guess x)
               x
               guess)))
  (sqrt-iter 1.0 x 0.0))

;; (improved-sqrt 10000)
;; (improved-sqrt 2)

;; Exercise 1.8
(defn square [x] (* x x))

(defn cube-root [x]
  (defn improve [guess x]
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
  (defn cube-root-iter
    [guess x prev-guess]
    (if (good-enough? guess prev-guess)
      guess
      (recur (improve guess x)
             x
             guess)))
  (cube-root-iter 1.0 x 0.0))

;; (cube-root 512)
;; (cube-root 8000)

;; Exercise 1.9: using the substitution model, illustrate the proces generated
;; by each procedur in ealuating (+ 4 5).

;; (defn exercise-1.9 []
;;   (defn + [a b]
;;     (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))
;;   (defn + [a b]
;;     (if (= a 0)
;;       b
;;       (+ (dec a) (inc b)))))


;; evaluating (+ 4 5) in the first version
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; conclusion: recursive process

;; evaluating (+ 4 5) in the second version
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; conclusion: iterative process

;; Exercise 1.10: Ackerman's function
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(A 1 10) ;; => 1024
(A 2 4) ;; => 65536
(A 3 3)

(defn exercise-1-10 []
  (defn f [n] (A 0 n)) ;; => f(x) = 2x
  (defn g [n] (A 1 n)) ;; => g(x) = 2^x
  (defn h [n] (A 2 n)) ;; => h(x) = ??
  (defn k [n] (* 5 n n)); => k(x) = 2x^2
  (println (f 8) (g 6) (h 4) (k 4)))

;; (exercise-1-10)

;; Exercise 1.11
;; Function defined by;
;; f(n) = n if n < 3 && f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3

;; recursive
(defn recursive-1-11 [x]
  (defn f [n]
    (cond (< n 3) n
          :else (+ (f (- n 1))
                   (* 2 (f (- n 2)))
                   (* 3 (f (- n 3))))))
  (f x))

;; (dotimes [n 10]
;;   (println (recursive-1-11 n)))


;; iterative
(defn iterative-1-11 [x]
  (defn f [n]
    (defn iter [i fi1 fi2 fi3]
      (if (= i (+ n 1))
        fi1
        (iter (+ i 1)
              (+ fi1 (* 2 fi2) (* 3 fi3))
              fi1
              fi2)))
    (if (< n 3)
      n
      (iter 3 2 1 0)))
  (f x))

;; (dotimes [n 10]
;;   (println (iterative-1-11 n)))

;; Exercise 1.12
(defn pascal-triangle [row]
  (defn row-recursive [n]
    (cond (= n 0) [1]
          (= n 1) [1 1]
          :else (let [prev-row (row-recursive (- n 1))]
                  (let [new-row (map-indexed (fn [index items]
                                               (+ (first items) (second items)))
                                             (partition 2 1 prev-row))]
                    (conj (apply vector (cons 1 new-row)) 1)))))
  (row-recursive row))

;; (dotimes [n 15]
;;   (println (pascal-triangle n)))
