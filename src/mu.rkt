#lang racket

;; proofs are lists that consist of the symbols m i u
(define-values (m i u) (values 'm 'i 'u))

(define start '(m i))

;; proofs must begin with m
(define (begins-with-m? proof)
  (eq? (car proof) 'm))

(define (ends-with-i? proof)
  (equal? (drop proof (- (length proof) 1)) '(i)))

(define (add-u proof)
  (append proof (list u)))

;; add u to any proof ending with i
(define (rule-add-u proof)
  (cond
    ((ends-with-i? proof) (list (add-u proof)))
    (else '())))

;; the symbols after the first m can be doubled
(define (rule-double proof)
  (list (append proof (cdr proof))))

(define (get-slice l offset len)
  (take (drop l offset) len))

;; len refer to the length of rep the replacement list and retl is an empty list
;; offset cannot nor should it be 0
(define (replace l offset rep len)
  (append (take l offset) rep (drop l (+ offset len))))

(define (del proof offset len)
  (append (take proof offset) (drop proof (+ offset len))))
  
(define (occurs-at? proof search index rl)
  (cond
    ((< (length proof) (length search)) rl)
    ((equal? (get-slice proof 0 (length search)) search)
     (occurs-at? (cdr proof) search (+ index 1) (append rl (list index))))
    (else (occurs-at? (cdr proof) search (+ index 1) rl))))

(define (triple-i-at? proof)
  (occurs-at? proof '(i i i) 0 '()))

(define (double-u-at? proof)
  (occurs-at? proof '(u u) 0 '()))

;; replace any set of triples i with u
(define (rep-iii proof index)
  (replace proof index '(u) 3))

;; generates all possible children of the triple i rule
(define (rep-iii-children proof indexes ret)
  (cond
    ((null? indexes) ret)
    ((null? (cdr indexes)) (append ret (list (rep-iii proof (car indexes)))))
    (else (rep-iii-children proof (cdr indexes) (append ret (list (rep-iii proof (car indexes))))))))

(define (rule-iii proof)
  (rep-iii-children proof (triple-i-at? proof) '()))

;; delete any set of uu
(define (del-uu proof index)
  (del proof index 2))

;; generates all possible children of the delete uu rule
(define (del-uu-children proof indexes ret)
  (cond
    ((null? indexes) ret)
    ((null? (cdr indexes)) (append ret (list (del-uu proof (car indexes)))))
    (else (del-uu-children proof (cdr indexes) (append ret (list (del-uu proof (car indexes))))))))

(define (rule-uu proof)
  (del-uu-children proof (double-u-at? proof) '()))

;; generates all possible children 
(define (get-children proof)
  (append (rule-uu proof) (rule-iii proof) (rule-double proof) (rule-add-u proof)))

;; generates all possible children for a list of proofs possible duplicates
(define (all-children proofs)
  (cond
    ((null? proofs) '())
    (else (append (get-children (car proofs)) (all-children (cdr proofs))))))

;; generates all possible children for a list of proofs possible duplicates
(define (all-children-dupless proofs)
  (remove-duplicates (all-children proofs)))

;; gets all the proofs a specific depth from the initial proof
;; proofs must be a list of proofs even if there is one proof
(define (get-depth depth proofs)
  (cond
    ((>= 0 depth) proofs)
    (else (get-depth (- depth 1) (all-children-dupless proofs)))))

;; all proofs up to a depth in a flattened list
(define (all-to-depth depth proofs)
  (cond
    ((>= 0 depth) '())
    (else (let ([dep (all-children-dupless proofs)])
            (append dep (all-to-depth (- depth 1) dep))))))

(define (all-to-depth-dupless depth proofs)
  (remove-duplicates (all-to-depth depth proofs)))

;; gives the length of a depth with a specific proof or proofs
(define (depth-len depth proofs)
  (length (get-depth depth proofs)))

(define (member-to-depth? find proofs depth)
  (not (eq? #f (member find (all-to-depth-dupless depth proofs)))))


(provide (all-defined-out))
