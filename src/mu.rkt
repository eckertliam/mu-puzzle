#lang racket

(require rackunit)

;; proofs are lists that consist of the symbols m i u
(define-values (m i u) (values 'm 'i 'u))

(define start '(m i))

;; proofs must begin with m
(define (begins-with-m? proof)
  (eq? (car proof) 'm))

(check-equal? (begins-with-m? '(m i u)) #t)
(check-equal? (begins-with-m? '(i u)) #f)

(define (ends-with-i? proof)
  (equal? (drop proof (- (length proof) 1)) '(i)))

(check-equal? (ends-with-i? '(m u i)) #t)
(check-equal? (ends-with-i? '(m u u)) #f)

(define (add-u proof)
  (append proof (list u)))

(check-equal? (add-u '(m i)) '(m i u))

;; add u to any proof ending with i
(define (rule-add-u proof)
  (cond
    ((ends-with-i? proof) (list (add-u proof)))
    (else '())))

(check-equal? (rule-add-u '(m i)) '((m i u)))

;; the symbols after the first m can be doubled
(define (rule-double proof)
  (list (append proof (cdr proof))))

(check-equal? (rule-double '(m i u)) '((m i u i u)))

(define (get-slice l offset len)
  (take (drop l offset) len))

(check-equal? (get-slice '(m i u i u i u i i i u i u u i i i u i u i u) 0 3) '(m i u))

;; len refer to the length of rep the replacement list and retl is an empty list
;; offset cannot nor should it be 0
(define (replace l offset rep len)
  (append (take l offset) rep (drop l (+ offset len))))

(check-equal? (replace '(m u u u u u u u u u u u u u u) 2 '(i i i i i) 5) '(m u i i i i i u u u u u u u u))

(define (del proof offset len)
  (append (take proof offset) (drop proof (+ offset len))))
  
(check-equal? (del '(m i u i u) 1 1) '(m u i u))

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

(define (double-u-at? proof)
  (occurs-at? proof '(u u) 0 '()))

(check-equal? (double-u-at? '(m i u u i u u i u i u u i i u i u u)) '(2 5 10 16))

;; replace any set of triples i with u
(define (rep-iii proof index)
  (replace proof index '(u) 3))

(check-equal? (rep-iii '(m i i i) 1) '(m u))

;; generates all possible children of the triple i rule
(define (rep-iii-children proof indexes ret)
  (cond
    ((null? indexes) ret)
    ((null? (cdr indexes)) (append ret (list (rep-iii proof (car indexes)))))
    (else (rep-iii-children proof (cdr indexes) (append ret (list (rep-iii proof (car indexes))))))))

(check-equal? (rep-iii-children '(m i i i i i i) (triple-i-at? '(m i i i i i i)) '()) '((m u i i i) (m i u i i) (m i i u i) (m i i i u)))

(define (rule-iii proof)
  (rep-iii-children proof (triple-i-at? proof) '()))

(check-equal? (rule-iii '(m i i i i i i)) '((m u i i i) (m i u i i) (m i i u i) (m i i i u)))

;; delete any set of uu
(define (del-uu proof index)
  (del proof index 2))

(check-equal? (del-uu '(m u u) 1) '(m))

;; generates all possible children of the delete uu rule
(define (del-uu-children proof indexes ret)
  (cond
    ((null? indexes) ret)
    ((null? (cdr indexes)) (append ret (list (del-uu proof (car indexes)))))
    (else (del-uu-children proof (cdr indexes) (append ret (list (del-uu proof (car indexes))))))))

(check-equal? (del-uu-children '(m u u i u u) (double-u-at? '(m u u i u u)) '()) '((m i u u) (m u u i)))

(define (rule-uu proof)
  (del-uu-children proof (double-u-at? proof) '()))

(check-equal? (rule-uu '(m i)) '())
(check-equal? (rule-uu '(m u u i u u)) '((m i u u) (m u u i)))

;; generates all possible children 
(define (get-children proof)
  (append (rule-uu proof) (rule-iii proof) (rule-double proof) (rule-add-u proof)))

(check-equal? (get-children '(m i)) '((m i i) (m i u)))
(check-equal? (get-children '(m i i i)) '((m u) (m i i i i i i) (m i i i u)))

;; generates all possible children for a list of proofs
(define (all-children proofs)
  (cond
    ((null? proofs) '())
    (else (append (get-children (car proofs)) (all-children (cdr proofs))))))

(check-equal? (all-children (get-children start)) '((m i i i i) (m i i u) (m i u i u)))

;; gets all the proofs a specific depth from the initial proof
;; proofs must be a list of proofs even if there is one proof
(define (get-depth depth proofs)
  (cond
    ((>= 0 depth) proofs)
    (else (get-depth (- depth 1) (all-children proofs)))))

(check-equal? (get-depth 2 '((m i))) '((m i i i i) (m i i u) (m i u i u)))

;; all proofs up to a depth in a flattened list
(define (all-to-depth depth proofs)
  (cond
    ((>= 0 depth) '())
    (else (let ([dep (all-children proofs)])
            (append dep (all-to-depth (- depth 1) dep))))))

(check-equal? (all-to-depth 2 (list start)) '((m i i) (m i u) (m i i i i) (m i i u) (m i u i u)))

;; gives the length of a depth with a specific proof or proofs
(define (depth-len depth proofs)
  (length (get-depth depth proofs)))

(check-equal? (depth-len 4 (list start)) 16)

(define (member-to-depth? find proofs depth)
  (not (eq? #f (member find (all-to-depth depth proofs)))))

(check-equal? (member-to-depth? '(m i i) (list start) 4) #t)
(check-equal? (member-to-depth? '(m u) (list start) 5) #f)

