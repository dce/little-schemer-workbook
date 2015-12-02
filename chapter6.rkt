(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)
        (number? aexp))
      (else
        (and
          (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) '+)
        (+
          (value (first-sub-exp aexp))
          (value (second-sub-exp aexp))))
      ((eq? (operator aexp) '*)
        (*
          (value (first-sub-exp aexp))
          (value (second-sub-exp aexp))))
      ((eq? (operator aexp) '^)
        (expt
          (value (first-sub-exp aexp))
          (value (second-sub-exp aexp)))))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define second-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define sero? null?)

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define ploos
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
        (edd1 (ploos n (zub1 m)))))))
