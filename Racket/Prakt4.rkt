#lang racket

;Зад. 1. Да се дефинира предикат (substr? a b),
;който проверява дали a е подниз на b, където a и b са естествени числа (например 123 е подниз на 5123783).

(define (suffix? a b)
 (cond [(= a 0) #t]
       [(= (remainder a 10) (remainder b 10)) (suffix? (quotient a 10) (quotient b 10))]
       [else #f]))

;(suffix? 123 654123)

(define (substr? a b)
(cond [(= b 0) (= a b)]
      [(suffix? a b) #t]
      [else (substr? a (quotient b 10))]))

;(substr? 888 5523412)

;Зад. 2. Дефинирайте следните функции:

;a). (my-identity x), функцията идентитет: връща каквото и дадете.

(define (my-identity x) x)

;(my-identity 2)

;б). (my-compose f g), която връща композицията на функциите f и g.

(define (my-compose f g)
  (λ(x) (f (g x))))

;((my-compose (λ(x) (* x 2)) (λ(x) (+ x 5))) 5)
;( ( my-compose( f(x) g(x) ) ) x )

;в). (my-negate p?), която приема предикат p? и връща предиката (not p?).

(define (my-negate p?)
(λ(x) (not (p? x))))

;((my-negate odd?) 10)

(define (p? x)
  (= x 0))

;((my-negate p?) 10)

;г). (my-curry f x), която приема многоаргумента функция f и първи аргумент x
; и връща функцията получена от частичното прилагане на x върху f. Използвайте вградената функция curry.

(define (my-curry f x)
  (λ(y z) (f x y z)))

((my-curry (λ(a b c) (+ a (* b c))) 1) 3 5)
; ( (my-curry (λ: f(a,b,c)), a) b, c )

;Зад. 3. Да се дефинира процедура от по-висок ред (difference F a b),
;която по дадени едноаргументна реална функция F и две реални числа a и b намира разликата F(b) - F(a).
;Да се оцени примерно орбъщение към процедурата.

(define (diff F a b)
  (- (F b) (F a)))

;(diff (λ (x) (* x 2)) 5 2)

;Зад. 4. Чрез използване на lambda израз да се дефинира процедурен обект,
;който е еквивалентен на f, ако имаме дефиницията (define (f x) (* 2 x)).

(define (f x)
  (* 2 x))

;аргументът е функция
(define f-2 (λ(x) (* 2 x)))

(f 10)
(f-2 10)

;Зад. 5. Да се дефинира процедура от по-висок ред (derive f eps),
;която намира първа производна на едноаргументната реална функция f с точност eps.

(define (derive f eps)
  (λ(x) (/ (- (f (+ x eps)) (f x)) eps)))

(define h (λ(x) (* x x)))

((derive h 1e-4) 2)
