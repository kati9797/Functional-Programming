#lang racket

; Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.
(define (mymin x y)
 (if(< x y)
  x
  y))
(mymin 7 3)

;Задача 2. Да се дефинира функцията inside? x a b, която проверява дали числото x се намира в затворения интервал [a, b].
(define (inside x a b)
  (if(< x a)
   #f
  (if(> x b)
   #f
   #t)))
(inside 6 1 10)

; Задача 3. Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа.
(define (myfunk a b)
  (define (sqr x)
    (* x ))
  (/ (+ (sqr a) (sqr b)) 2))
(myfunk 4 5)

; Факториел
(define (fact n)
  (if(= n 0)
  1
  (* n (fact(- n 1)))))
(fact 4)

;Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи. 
;Да се напише и итеративно решение.
;0 1 1 2 4
;1 0 0 -> i = 0 cur = 0
;0 1 1 -> i = 1 cur = 1
;1 1 2 -> i = 2 cur = 1
;1 2 3 -> i = 3 cur = 2

(define (myfib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (myfib(- n 1)) (myfib(- n 2)))]))
(myfib 7)

(define (fib-iter n)
(define (helper prev cur i)
  (if (= i n)
      cur
      (helper cur (+ cur prev) (+ i 1))))
  (helper 1 0 0))
(fib-iter 7)
(fib-iter 500)
