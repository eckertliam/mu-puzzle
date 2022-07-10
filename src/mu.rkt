#lang racket

(require rackunit)

;; proofs are lists that consist of the symbols m i u
(define-values (m i u) (values 'm 'i 'u))

;; proofs must begin with m
(define (begins-with-m? proof)
  (eq? (car proof) 'm))

(check-equal? (begins-with-m? '(m i u)) #t)
(check-equal? (begins-with-m? '(i u)) #f)

;; u can be added to end of any proof ending with i
(define (ends-with-i? proof)
  (cond
    ((null? (cdr proof)) (eq? (car proof) 'i))
    (else (ends-with-i? (cdr proof)))))

(check-equal? (ends-with-i? '(m u i)) #t)
(check-equal? (ends-with-i? '(m u u)) #f)

(define (add-u proof)
  (append proof (list u)))

(check-equal? (add-u '(m i)) '(m i u))

;; the symbols after the first m can be doubled
(define (double-cdr proof)
  (append proof (cdr proof)))

(check-equal? (double-cdr '(m i u)) '(m i u i u))

(define (get-slice l offset len)
  (take (drop l offset) len))

(check-equal? (get-slice '(m i u i u i u i i i u i u u i i i u i u i u) 0 3) '(m i u))

;; len refer to the length of rep the replacement list and retl is an empty list
;; offset cannot nor should it be 0
(define (replace l offset rep len retl)
  (cond
    ((<= offset 0)
     (cond
       ((eq? (* -1 offset) len) (append retl l))
       (else (replace (cdr l) (- offset 1) (cdr rep) len (append retl (list (car rep)))))))
    (else (replace (cdr l) (- offset 1) rep len (append retl (list (car l)))))))

(check-equal? (replace '(m u u u u u u u u u u u u u u) 2 '(i i i i i) 5 '()) '(m u i i i i i u u u u u u u u))

;; uses a modified replace
(define (remove l offset len retl)
  (cond
    ((not (eq? offset 0)) (remove (replace l offset (make-list len '()) len '()) 0 len '()))
    (else
     (cond
       ((null? (cdr l)) (append retl (list (car l))))
       ((null? (car l)) (remove (cdr l) offset len retl))
       (else (remove (cdr l) offset len (append retl (list (car l)))))))))

(check-equal? (remove '(m i u i u) 1 1 '()) '(m u i u))

(define (occurs-at? proof search index rl)
  (cond
    ((< (length proof) (length search)) rl)
    ((equal? (get-slice proof 0 (length search)) search)
     (occurs-at? (cdr proof) search (+ index 1) (append rl (list index))))
    (else (occurs-at? (cdr proof) search (+ index 1) rl))))

(check-equal? (occurs-at? '(m i u i u i i i) '(i u) 0 '()) '(1 3))

(define (triple-i-at? proof)
  (occurs-at? proof '(i i i) 0 '()))

(check-equal? (triple-i-at? '(m i u i i i u i i i u i i i)) '(3 7 11))

(define (double-uu-at? proof)
  (occurs-at? proof '(u u) 0 '()))

(check-equal? (double-uu-at? '(m i u u i u u i u i u u i i u i u u)) '(2 5 10 16))

(define (triple-i-children proof occurs)
  (let ([rl '()])
    (cond
      ((null? (cdr occurs)) rl)
      (else (triple-i-children proof (append rl (replace proof (car occurs) '(i i i) 3 '())) (cdr occurs))))))

(check-equal? (triple-i-children '(m i i i) (triple-i-at? '(m i i i))) '((m u)))
