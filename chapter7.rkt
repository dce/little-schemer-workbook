(define sset?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (sset? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
        (cons
          (car lat)
          (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
        (and
          (member? (car set1) set2)
          (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and
      (subset? set1 set2)
      (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
        (or
          (member? (car set1) set2)
          (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
        (cons
          (car set1)
          (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
        (union (cdr set1) set2))
      (else
        (cons
          (car set1)
          (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
       (intersect
         (car l-set)
         (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (cdr l)) #f)
      ((null? (cdr (cdr l))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define firsts
  (lambda (pairs)
    (cond
      ((null? pairs) '())
      (else
        (cons
          (first (car pairs))
          (firsts (cdr pairs)))))))

(define fun?
  (lambda (rel)
    (sset? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
        (cons
          (revpair (car rel))
          (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define fullfun?
  (lambda (rel)
    (sset? (firsts (revrel rel)))))
