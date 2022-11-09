#lang racket
(require math/number-theory)

;Define a procedure that removes an element from a list.

(define (erase xs x)
  (cond [(empty? xs) '()]
        [(equal? (first xs) x) (erase (rest xs) x)]
        [else (cons (first xs) (erase (rest xs) x))]))

;(equal? (erase '(1 1 1 2) 1) '(2))
;(equal? (erase '(2 5 6) 1) '(2 5 6))
;(equal? (erase '(1) 1) '())
;(equal? (erase '(1 2 1 1) 1) '(2))
;(equal? (erase '("CNN" "RNN" "GAN" "RNN") "RNN") '("CNN" "GAN"))

;----------------------------------------------------------------------------

;Define a procedure longest-ascending-sub xs that returns the longest sublist
;(sequence of consecutive elements) that is sorted in ascending order from xs.

(define (longest-ascending-sub xs)
  (define (helper curr res xs)
    (cond [(and (empty? xs) (> (length res) (length curr))) (reverse res)]
          [(and (empty? xs) (< (length res) (length curr))) (reverse curr)]
          [(empty? curr) (helper (cons (first xs) curr) res (rest xs))]
          [(<= (first curr) (first xs)) (helper (cons (first xs) curr) res (rest xs))]
          [(and (> (first curr) (first xs)) (> (length curr) (length res))) (helper '() curr xs)]
          [else (helper '() res xs)]))
  (helper '() '() xs))

;(longest-ascending-sub '(1 0 5))
;(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
;(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
;(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
;(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
;(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))

;-----------------------------------------------------------------------------

;Define a procedure (set-union xs ys) that takes two sets of numbers and returns their union.
;It (the union) must be sorted in ascending order!

(define (set-union xs ys)
  (cond [(and (empty? xs) (empty? ys)) '()]
        [(and (empty? xs) (not (empty? ys))) (cons (first ys) (set-union xs (rest ys)))]
        [(and (not (empty? xs)) (empty? ys)) (cons (first xs) (set-union (rest xs) ys))]
        [(< (first xs) (first ys)) (cons (first xs) (set-union (rest xs) ys))]
        [(> (first xs) (first ys)) (cons (first ys) (set-union xs (rest ys)))]
        [(= (first xs) (first ys)) (cons (first xs) (set-union (rest xs) (rest ys)))]))

;(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
;(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))

;(foldl cons '() '(1 2 3 4))
;(foldr cons +0 '(1 2 3 4))
;(foldl (λ (a b result) (* result (- a b)))
;       1
;       '(1 2 3)
;       '(4 5 6))

;-----------------------------------------------------------------------------

;Define procedures that accept a list of digits and return the number that is build by traversing the list from right to left.
;Create two versions - one that utilizes folding, and another that does recursion

(foldl (λ (a res) (+ (* 10 res) a))
       0
       (foldl cons '() '(1 2 3)))

(define (rev xs)
  (define (helper xs res)
    (if (empty? xs)
        res
        (helper (rest xs) (cons (first xs) res))))
  (helper xs '()))

;(rev '(1 2 3 4))

(define (rev-lin-iter xs)
  (define (helper rev res)
    (if (empty? rev)
        res
        (helper (rest rev) (+ (* 10 res) (first res)))))

  (helper (rev xs) 0))

;(= (rev-lin-iter '(1 2 3)) 321)
;(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)

;-----------------------------------------------------------------------------

;Define a procedure (insert-at x i xs) that inserts an element at a given index in a list.

(define (insert-at x i xs)
  (cond [(> i (length xs)) "Invalid index"]
        [(and (empty? xs) (= i 0)) (list x)]
        [(= i 0) (cons x xs)]
        [(> i 0) (cons (first xs) (insert-at x (- i 1) (rest xs)))]))

;(insert-at 1 0 '())
;(equal? (insert-at 1 0 '()) '(1))
;(equal? (insert-at 1 0 '(2)) '(1 2))
;(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
;(equal? (insert-at 1 0 '()) '(1))
;(equal? (insert-at 1 0 '(2)) '(1 2))
;(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
;(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
;(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
;(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))
;(insert-at 7 4 '(1 2 3))

;-----------------------------------------------------------------------------

;According to the fundamental theorem of arithmentics every natural number greater than 2 can be expressed as a product of prime numbers.
;Define a procedure that returns the sorted list of prime factors of a natural number.

(define (factorize n)
(define (helper res i curr)
  (cond [(< n 2) "Invalid n"]
        [(= curr 1) res]
        [(and (= 0 (remainder curr i)) (prime? i)) (helper (flatten (list (cons i res))) i (quotient curr i))]
        [else (helper res (- i 1) curr)]))
   (helper '() n n))

;(equal? (factorize 2) '(2))
;(equal? (factorize 6) '(2 3))
;(equal? (factorize 13) '(13))
;(equal? (factorize 123) '(3 41))
;(equal? (factorize 152) '(2 2 2 19))
;(equal? (factorize 12356498) '(2 7 11 19 41 103))

;-----------------------------------------------------------------------------

;By using num-to-xs and xs-to-num define a procedure that sorts a number in descending order.

(define (num-to-xs n)
  (define (helper leftover)
    (if (< leftover 10)
        (list leftover)
        (cons (remainder leftover 10) (helper (quotient leftover 10)))
        )
    )
  (reverse (helper n))
  )

(define (xs-to-num xs)
  (foldl (λ (x acc) (+ (* acc 10) x)) 0 xs)
  )

(define (sort-n n)
  (xs-to-num (sort (num-to-xs n) >)))

;(= (sort-n 1714) 7411)
;(= (sort-n 123450) 543210)
;(= (sort-n 123405) 543210)
;(= (sort-n 123045) 543210)
;(= (sort-n 120345) 543210)
;(= (sort-n 102345) 543210)
;(= (sort-n 8910) 9810)
;(= (sort-n 321) 321)
;(= (sort-n 29210) 92210)
;(= (sort-n 1230) 3210)
;(= (sort-n 55345) 55543)
;(= (sort-n 14752) 75421)
;(= (sort-n 329450) 954320)
;(= (sort-n 9125) 9521)

;-----------------------------------------------------------------------------

;By using recursion define a procedure that implements the list-ref procedure we discussed in class.
(define (my-list-ref xs k)
  (define (helper i xs)
    (cond [(or (> k (- (length xs) 1)) (< k 0)) "Invalid index"]
          [(= i k) (first xs)]
          [else (helper (+ i 1) (rest xs))]))
  (helper 0 xs))

;(= (my-list-ref '(1 2 3) 0) 1)
;(= (my-list-ref '(1 2 3) 1) 2)
;(equal? (my-list-ref '("Hello" 2 ("nested list")) 0) "Hello")
;(my-list-ref '(1 2 3) -100) 
