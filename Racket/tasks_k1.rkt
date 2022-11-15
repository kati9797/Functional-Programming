#lang racket

;Задачи от контролни

;Да се дефинира функция (get-distribution n), която приема неотрицателно
;цяло число n и връща списък с елементи от вида (<цифра> . <брой срещания
;в n^2>), представящ разпределението на цифрите на n^2. Резултатът да е
;сортиран спрямо цифрите на n^2.

(define (count-digit n k)
  (define (helper cnt n)
    (cond [(and (= n 0) (= k 0)) 1]
          [(= n 0) cnt]
          [(= (remainder n 10) k) (helper (+ cnt 1) (quotient n 10))]
          [else (helper cnt (quotient n 10))]))
  (helper 0 n))

(define (get-distribution n)
  (define (helper x res)
    (cond [(= n 0) (cons (1 0))]
          [(= x 0) res]
          [else (helper (quotient x 10) (cons (cons (remainder x 10) (count-digit (expt 123 2) (remainder x 10))) res))]))
  (remove-duplicates (helper (expt n 2) '())))

(define (sort-car)
  (λ(a b) (< (car a) (car b))))
(get-distribution 123)
(sort (get-distribution 123) (sort-car))

;Даден е списък от непразни списъци. Елементите на дадения списък са такива, че
;ако сортирате списъка от техните дължини, ще получите списък с естествени числа.
;Те биха формирали строго нарастваща редица от поредни естествени числа, в
;която едно число липсва.
;Да се дефинира функция (get-missing-length xss), която връща липсващото
;число за списъка xss. Ако някой от елементите на xss е празен или списъкът xss
;е празен, да се връща грешката “Empty list!”.

(define (convert-to-num xs)
  (define (helper res xs len)
    (if (= len 0)
        res
        (helper (cons (length (first xs)) res) (rest xs) (- len 1))))
  (sort (helper '() xs (length xs)) <))

(define (get-missing-length xs)
  (define last-dig (last (convert-to-num xs)))
  (define first-dig (first (convert-to-num xs)))
  (define (helper previous curr xs)
    (cond [(= curr (+ last-dig 1)) "Not missing digits"]
          [(= curr 0) "Empty list"]
          [(not (= (+ previous 1) curr)) (+ previous 1)]
          [else (helper curr (first xs) (rest xs))]))
  (helper (- first-dig 1) first-dig (rest (convert-to-num xs))))

;(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9)))
;(get-missing-length '(("a" "a" "a") ("a" "a") ("a" "a" "a" "a") ("a") ("a" "a" "a" "a" "a" "a")))

;Да се дефинира функция (persistence n), която приема естествено число n и
;връща точкова двойка от вида ‘(ys . x). Първият елемент на резултата е списък,
;чийто първи елемент е равен на произведението на цифрите на n, а всеки следващ
;е равен на произведението на цифрите на предходния до получаването на
;едноцифрено произведение, на което е равен последният елемент на ys. Вторият
;елемент на резултата (т.е. x) е равен на дължината на списъка ys.

(define (product n)
  (define (helper res n)
    (if (< n 10)
        (* res n)
        (helper (* res (remainder n 10)) (quotient n 10))))
  (helper 1 n))

(define (persistence n)
  (define (helper curr res)
    (if (< curr 10)
        (flatten (cons res curr))
        (helper (product curr) (flatten (cons res curr)))))
  (define len (length (helper (product n) '())))
  (cons (helper (product n) '()) len))

;(persistence 39)
;(persistence 126)
;(persistence 4)
;(persistence 999)

;Да се дефинира процедура (kth-max-min xs), която приема списък
;от цели числа и връща процедура с параметър естествено число k – такава, че
;оценката на израза ((kth-max-min xs) k) e k-тото по големина отрицателно
;число в xs. Ако такова число не съществува, да се връща грешката “No such
;number”.

(define (negative)
  (λ(x) (< x 0)))

(define (kth-max-min xs)
  (define new-xs (remove-duplicates (sort (filter (negative) xs) <)))
  (λ(k) (if (> k (length new-xs))
            "No such number"
            (list-ref new-xs (- k 1)))))

;((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2)
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3)

;Да се дефинира процедура (shuffle xs), която получава списък от
;2*n елемента във вида '(x1 x2 ... xn y1 y2 ... yn) и връща списък във вида
;'(x1 y1 x2 y2 ... xn yn).

(define (shuffle xs)
  (define len (length xs))
  (define (helper first-xs second-xs res)
    (if (and (empty? first-xs) (empty? second-xs))
        res
        (helper (rest first-xs) (rest second-xs) (flatten (cons res (cons (first first-xs) (first second-xs)))))))
  (helper (take xs (/ len 2)) (take (drop xs (/ len 2)) (/ len 2)) '()))

;(shuffle '(2 5 1 3 4 7))
;(shuffle '(1 2 3 4 4 3 2 1))
;(shuffle '(1 1 2 2))

;Да се дефинира предикат (triangular? mat), който получава
;квадратна числова матрица, представена като списък от списъци, и проверява дали
;тя е горно триъгълна, т.е. дали всички елементи под главния ѝ диагонал са нули.

(define (first-k-zeros? xs k)
  (define (helper xs)
    (cond [(empty? xs) #t]
          [(= 0 (first xs)) (helper (rest xs))]
          [else #f]))
  (helper (take xs k)))

(define (triangular? mat)
  (define (helper xs k)
    (cond [(empty? xs) #t]
          [(first-k-zeros? (first xs) k) (helper (rest xs) (+ k 1))]
          [else #f]))
  (helper (drop mat 1) 1))

;(triangular? '((1 2 3)
; (0 5 6)
; (0 0 9)))
;(triangular? '((0 2 3)
; (0 0 6)
; (1 0 0)))
;(triangular? '((1 2 3)
; (1 5 6)
; (0 0 9)))
;(triangular? '((1 2 3 4)
; (0 5 6 7)
; (0 0 8 9)
; (0 0 0 9)))

;Да се дефинира процедура от по-висок ред (trailing-zeros n), която връща
;анонимна функция, която приема предикат p и проверява дали p е в сила за броя
;на влачещите нули в n!

(define (trailing-zeros n)
  (define (helper sum k)
    (if (>= (expt 5 k) n)
        sum
        (helper (+ sum (quotient n (expt 5 k))) (+ k 1))))
  (λ(p?) (p? (helper 0 1))))

;((trailing-zeros 6) even?)
;((trailing-zeros 1000) even?)
;((trailing-zeros 100000) even?)
;((trailing-zeros 1000000000) even?)
