#lang racket

(require rackunit)

(define split-string
  (lambda (str)
    (map string (string->list str))))

(check-equal? (split-string "me") '("m" "e"))

(define last-element
  (lambda (l)
    (cond
      ((null? (cdr l)) (car l))
      (else (last-element (cdr l))))))

(check-equal? (last-element '(1 2 3 4)) '4)

(define can-add-u?
  (lambda (l)
    (cond
      ((eq? (last-element l) "i") #t)
      (else #f))))

(check-equal? (can-add-u? '("m" "i")) '#t)
(check-equal? (can-add-u? '("m" "u")) '#f)

(define remove-last
  (lambda (l)
    (cond
      ((null? l)(error 'l "is an empty list"))
      ((null? (cdr l)) '())
      (else (cons (car l) (remove-last (cdr l)))))))

(check-equal? (remove-last '(1 2 3)) '(1 2))

(define add-u
  (lambda (l)
    (cond
      ((null? l)
       (error 'l "is an empty lsit"))
      ((can-add-u? l)
       (let ([rl (remove-last l)])
         (append rl (list "u"))))
      (else (error 'l "is unkwown")))))

(check-equal? (add-u '("m" "i" "i")) '("m" "i" "u"))

