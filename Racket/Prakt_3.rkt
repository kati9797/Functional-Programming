#lang racket
(require racket/trace)

;Задача 1. Да се дефинира функция (sum-prime-divisors n),
;която намира сумата на всички прости делители на едно число n.

(define (prime? n)
  (define (helper d)
  (cond [(= n d) #t]
        [(= 0 (remainder n d)) #f]
        [else (helper (+ d 1))]))
  (and (> n 1) (helper 2)))

(define (sum-prime-divisions n)
  (define (helper d sum)
    (cond [(> d n) sum]
          [(and (prime? d) (= 0 (remainder n d)))  (helper (+ d 1) (+ sum d))]
          [else (helper (+ d 1) sum)]))
  (trace helper)
  (helper 2 0))

;(sum-prime-divisions 9)

;Задача 2. Да се дефинира функция (pow x n), която генерира линейно рекурсивен процес
;и намира x на степен n, където x е реално, а n - естествено число.

(define (pow x n)
  (define (helper i power)
    (if (= i n)
        power
        (helper (+ i 1) (* power x))))
  (helper 0 1))

;(trace pow)
;(pow 2 4)

;Задача 3. Да се дефинира функция (count-оccurences d n),
;намираща броя на срещанията на дадена цифра d в записа на число n.

(define (count-occurences d n)
  (define (helper n cnt)
    (cond [(= n 0) cnt]
          [(= d (remainder n 10)) (helper (quotient n 10) (+ cnt 1))]
          [else (helper (quotient n 10) cnt)]))
  (helper n 0))

;(count-occurences 2 22222)

;Задача 4. Да се дефинира предикат (ascending? n), който връща истина,
;ако цифрите на дадено естествено число n са в нарастващ ред от първата към последната.

(define (ascending? n)
  (cond [(< n 10) #t]
        [(>= (remainder n 10) (remainder (quotient n 10) 10)) #f]
        (else (ascending? (quotient n 10)))))

;(ascending? 98765)
;(ascending? 98789)

;Задача 5.Да се дефинира функцията (perfect-number? n),
;която проверява дали числото n e съвършено, т.е. дали е равно на сбора на делителите си.
;Ctrl + \ ----> lambda

(define (filtered-sum p? a b)
  (cond [(> a b) 0]
        [(p? a) (+ a (filtered-sum p? (+ a 1) b))]
        [else (filtered-sum p? (+ a 1) b)]))

(define (perfect-number? n)
  (define (divisor? d)
    (= 0 (remainder n d)))
  (= n (filtered-sum divisor? 1 (- n 1))))

;(perfect-number? 6)
;(perfect-number? 27)
;(perfect-number? 28)
;(perfect-number? 8128)

;Задача 6. По зададени x и n, да се дефинира функция (calc-sum x n),
;която пресмята сумата: 1 + x + x^2 + x^3 + ... + x^n. Използвайте не повече от n на брой умножения.

(define (calc-sum x n)
  (define (helper prev i sum)
    (if (> i n)
        (+ sum prev)
        (helper (* x prev) (+ i 1) (+ sum prev))))
  (helper 1 1 0))

;(calc-sum 2 0)
;(calc-sum 2 1)
