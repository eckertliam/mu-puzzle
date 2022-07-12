#lang racket

(require rackunit)

(require "mu.rkt")

(check-equal? (begins-with-m? '(m i u)) #t)
(check-equal? (begins-with-m? '(i u)) #f)

(check-equal? (ends-with-i? '(m u i)) #t)
(check-equal? (ends-with-i? '(m u u)) #f)

(check-equal? (rule-add-u '(m i)) '((m i u)))

(check-equal? (rule-double '(m i u)) '((m i u i u)))

(check-equal? (get-slice '(m i u i u i u i i i u i u u i i i u i u i u) 0 3) '(m i u))

(check-equal? (replace '(m u u u u u u u u u u u u u u) 2 '(i i i i i) 5) '(m u i i i i i u u u u u u u u))

(check-equal? (occurs-at? '(m i u i u i i i) '(i u) 0 '()) '(1 3))

(check-equal? (triple-i-at? '(m i u i i i u i i i u i i i)) '(3 7 11))

(check-equal? (double-u-at? '(m i u u i u u i u i u u i i u i u u)) '(2 5 10 16))

(check-equal? (rep-iii '(m i i i) 1) '(m u))

(check-equal? (del '(m i u i u) 1 1) '(m u i u))

(check-equal? (add-u '(m i)) '(m i u))

(check-equal? (rep-iii-children '(m i i i i i i) (triple-i-at? '(m i i i i i i)) '()) '((m u i i i) (m i u i i) (m i i u i) (m i i i u)))

(check-equal? (rule-iii '(m i i i i i i)) '((m u i i i) (m i u i i) (m i i u i) (m i i i u)))

(check-equal? (del-uu '(m u u) 1) '(m))

(check-equal? (del-uu-children '(m u u i u u) (double-u-at? '(m u u i u u)) '()) '((m i u u) (m u u i)))

(check-equal? (rule-uu '(m i)) '())
(check-equal? (rule-uu '(m u u i u u)) '((m i u u) (m u u i)))

(check-equal? (get-children '(m i)) '((m i i) (m i u)))
(check-equal? (get-children '(m i i i)) '((m u) (m i i i i i i) (m i i i u)))

(check-equal? (all-children (get-children start)) '((m i i i i) (m i i u) (m i u i u)))

(check-equal? (get-depth 2 '((m i))) '((m i i i i) (m i i u) (m i u i u)))

(check-equal? (all-to-depth 2 (list start)) '((m i i) (m i u) (m i i i i) (m i i u) (m i u i u)))

(check-equal? (depth-len 4 (list start)) 15)

(check-equal? (member-to-depth? '(m i i) (list start) 4) #t)
(check-equal? (member-to-depth? '(m u) (list start) 5) #f)

(check-equal? (<
               (length (all-children-dupless (all-to-depth 5 (list start))))
               (length (all-children (all-to-depth 5 (list start)))))
              #t)

(check-equal? (<
               (length (all-to-depth-dupless 5 (list start)))
               (length (all-to-depth 5 (list start))))
              #t)
