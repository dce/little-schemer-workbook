(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
