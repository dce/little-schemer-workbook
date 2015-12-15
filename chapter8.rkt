(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else
          (cons
            (car l)
            ((rember-f test?) a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))


(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons new lat))
        (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons old (cons new (cdr lat))))
        (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((eq? old (car lat)) (seq new old (cdr lat)))
        (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(define subst
  (insert-g
    (lambda (new old l)
      (cons new l))))

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a '+) o+)
      ((eq? a 'x) x)
      (else o^))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function (operator nexp))
          (value (first-sub-exp nexp))
          (value (second-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((eq? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else
          (cons
            (car lat)
            ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multirember test? (cdr lat)))
      (else
        (cons
          (car lat)
          (multirember test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
        (col '() '()))
      ((eq? (car lat) a)
        (multirember&co a
          (cdr lat)
          (lambda (newlat seen)
            (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co a
          (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
        (col '() 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLR&co new oldL oldR
          (cdr lat)
          (lambda (newlat L R)
            (col (cons new (cons oldL newlat) (add1 L) R)))))
      ((eq? (car lat) oldR)
        (multiinsertLR&co new oldL oldR
          (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR (cons new newlat) L (add1 R))))))
      (else
        (multiinsertLR&co new oldL oldR
          (cdr lat)
          (lambda (newlat L R)
            (col (cons (car lat) newlat) L R)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((even? (car l))
            (cons (car l) (evens-only* (cdr l))))
          (else (evens-only* (cdr l)))))
      (else
        (cons
          (evens-only* (car l))
          (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
        (col '() 1 0))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (evens-only*&co
              (cdr l)
              (lambda (newlat p s)
                (col (cons (car l) newlat) (* (car l) p) s))))
          (else
            (evens-only*&co
              (cdr l)
              (lambda (newlat p s)
                (col newlat p (+ s (car l))))))))
      (else
        (evens-only*&co
          (car l)
          (lambda (al ap as)
            (evens-only*&co
              (cdr l)
              (lambda (dl dp ds)
                (col (cons al dl)
                  (* ap dp)
                  (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))
