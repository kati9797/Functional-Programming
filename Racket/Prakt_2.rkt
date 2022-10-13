#lang racket

;Задача 1. Да се напише функция mygcd a b, която връща НОД(a, b).
;Алгоритъм на Евклид
;а=0 -> b
;gcd(b%a,a)

(define (mygcd a b)
  (if (= a 0)
      b
      (mygcd (remainder b a) a)))

;(mygcd 10 15)

;Задача 2. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.

(define (mymaxdivisor x)
  (define (helper d)
    (if (= 0 (remainder x d))
        d
        (helper (- d 1))))
  (helper (- x 1)))

;(mymaxdivisor 20)

;Задача 3. Да се дефинира функция sum-odds a b, която намира сумата на нечетните числа в затворения интервал [a, b].

;(odd? 5)

(define (sum-odds a b)
  (define (helper sum a)
    (cond [(> a b) sum]
          [(odd? a) (helper (+ sum a) (+ a 1))]
          [else (helper sum (+ a 1))]))
  (helper 0 a))

;[1,9] -> 1+3+5+7+9=25
;(sum-odds 1 9)

;Задача 4. Да се дефинира предикат prime? n, който проверява дали естественото число n е просто.
;n>1 и най-големият общ делител < n e 1

(define (prime? n)
  (and (> n 1) (= 1 (mymaxdivisor n))))

;(prime? 2)
;(prime? 4)
;(prime? 7)

;Задача 5. Да се дефинира функция count-palindromes a b, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.

(define (reverse n)
  (define (helper res k)
    (if (< k 10)
        (+ (* res 10) k)
        (helper (+ (* res 10) (remainder k 10)) (quotient k 10))))
  (helper 0 n))

;(reverse 123)

(define (palindrome? n)
  (if (= n (reverse n))
      #t
      #f))

;(palindrome? 12321)
;(palindrome? 12567)

(define (count-palindromes a b)
  (define (helper cnt a)
    (cond [(> a b) cnt]
          [(palindrome? a) (helper (+ cnt 1) (+ a 1))]
          [else (helper cnt (+ a 1))]))
  (helper 0 a))

;(count-palindromes 10 20)

;Задача 6. Да се дефинира функция count-divisors n, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число n.

(define (count-divisors n)
  (define (helper cnt i)
    (cond [(> i n) cnt]
          [(= 0 (remainder n i)) (helper (+ cnt 1) (+ i 1))]
          [else (helper cnt (+ i 1))]))
  (helper 0 1))

;(count-divisors 9)
