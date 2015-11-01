(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((list? (car l)) #f)
      (else (lat? (cdr l))))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))
