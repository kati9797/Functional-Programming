#lang racket

;first

(define (sum-digits-div x k)
(define (helper sum x)
  (if (< x 10)
      (+ sum x)
      (helper (+ sum (remainder x 10)) (quotient x 10))))
  (if (= 0 (remainder (helper 0 x) k))
      #t
      #f))

(define (count-specials k a b)
(define (helper i cnt)
  (cond [(> i b) cnt]
        [(and (= 0 (remainder i k)) (sum-digits-div i k)) (helper (+ i 1) (+ cnt 1))]
        [else (helper (+ i 1) cnt)]))
  (helper a 0))

;(count-specials 8 100 200)
;(count-specials 5 10 100) 
;(count-specials 8 100 200) 
;(count-specials 15 1000 2000)

;second

(define (count-digits x)
  (define (helper x cnt)
    (if (< x 10)
        (+ cnt 1)
        (helper (quotient x 10) (+ cnt 1))))
  (helper x 0))

(define (reverse n)
  (define (helper res n)
    (if (< n 10)
        (+ res n)
        (helper (* 10 (+ res (remainder n 10))) (quotient n 10))))
  (helper 0 n))

(define (rot n)
  (define (helper1)
  (remainder (reverse n) 10))

  (define (helper2)
    (if (= 0 (remainder n 10))
        #t
        #f))
  
  (define (helper3 ind)
  (* (reverse (* 10 (quotient (reverse n) (* (+ 1 ind) 10)))) 10))

  (if(helper2)
     (+ (helper1) (* 10 (helper3 0)))
     (+ (helper1) (helper3 0))))

(define (pow x n)
  (define (helper i power)
    (if (= i n)
        power
        (helper (+ i 1) (* power x))))
  (helper 0 1))

(define (curr-shift prev ind cnt)
  (if(= 0 (remainder (quotient (reverse prev) (pow 10 ind)) 10))
  (+ (* (reverse (remainder (reverse prev) (pow 10 ind))) (pow 10 (- cnt ind))) (* 10 (reverse (quotient (reverse prev) (pow 10 ind)))))
  (+ (* (reverse (remainder (reverse prev) (pow 10 ind))) (pow 10 (- cnt ind))) (rot (reverse (quotient (reverse prev) (pow 10 ind)))))))

(define (max-rot n)
  (define max (rot n))
  (define cnt (count-digits n))
  (define (helper prev max ind)
    (cond [(= ind (- cnt 1)) max]
          [(> (curr-shift prev ind cnt) max) (helper (curr-shift prev ind cnt) (curr-shift prev ind cnt) (+ ind 1))]
          [else (helper (curr-shift prev ind cnt) max (+ ind 1))]))
    (helper max max 1))

;(max-rot 56789) 
;(max-rot 12490)
;(max-rot 38458215) 
;(max-rot 195881031) 
;(max-rot 896219342) 
;(max-rot 69418307)
;(max-rot 257117280)
