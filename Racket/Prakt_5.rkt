#lang racket

;Задача 1. Да се дефинира функция, която намира дължината на списък.

(define lst (list 1 2 3 4))
(length lst)

(define (count xs)
  (if (empty? xs)
      0
      (+ 1 (count (rest xs)))))

(count lst)

;Задача 2. Да се дефинира функция, която проверява дали даден елемент се съдържа в списък.

(define (contains? x xs)
  (cond [(empty? xs) #f]
        [(equal? x (first xs)) #t]
        [else (contains? x (rest xs))]))

(contains? 7 '(0 1 2 3))
(contains? 2 '(1 2 3 4))
(contains? '(1 2) '(9 0 '(1 2)))

;Задача 3. Да се дефинира функция, която добавя елемент на зададена позиция в списък.

(define (add x ind xs)
  (cond [(empty? xs) (list x)]
        [(= ind 0) (cons x xs)]
        [else (cons (car xs) (add x (- ind 1) (cdr xs)))]))

(add 3 2 '(0 1 2 4))

;Задача 4. Да се дефинира функция, която намира най-малкия елемент на списък.

(define (min-el xs)
    (if (empty? (rest xs))
        (first xs)
        (min (first xs) (min-el (rest xs)))))

(min-el '(9 3 4 1 6 2 2))
(apply min '(9 3 4 1 6 2 2))

;Задача 5. Да се дефинира функция, която изтрива първото срещане на даден елемент в списък.

(define (erase x xs)
 (cond  [(empty? xs) '()]
        [(equal? (first xs) x) (rest xs)]
        [else (cons (first xs) (erase x (rest xs)))]))

(erase 1 '(9 3 4 1 6 2 2))

;Задача 6. Да се дефинира функция, която изтрива всички срещания на даден елемент на списък.

(define (erase-all x xs)
 (cond  [(empty? xs) '()]
        [(equal? (first xs) x) (erase-all x (rest xs))]
        [else (cons (first xs) (erase-all x (rest xs)))]))

(erase-all 1 '(9 3 4 1 6 1 2))

;Задача 7. Да се напише функция, която конкатенира два списъка.

(define (concat xs ys)
  (if (empty? xs)
      ys
      (cons (first xs) (concat (rest xs) ys))))

(concat '(1 2 3 4) '(5 6 7 8))

;Задача 8. Да се напише функция, която обръща даден списък.

(define (reverse xs)
  (define (helper xs res)
    (if (empty? xs)
        res
        (helper (rest xs) (cons (first xs) res))))
    (helper xs '()))

(reverse '(1 2 3 4))
