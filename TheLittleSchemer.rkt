#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))
    

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

;;;03 Cons the Magnificent

(define rember0
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat)) (cdr lat))
              (else (rember0 a (cdr lat))))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

;;;04 Numbers Games

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define a+
  (lambda (n n2)
    (cond
      ((zero? n2) n)
      (else (add1 (a+ n (sub1 n2)))))))

(define s-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (s- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (a+ (car tup) (addtup (cdr tup)))))))

(define ax
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (a+ n (ax n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons (a+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define tup+2
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (a+ (car tup1) (car tup2)) (tup+2 (cdr tup1) (cdr tup2)))))))

(define a>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (a> (sub1 n) (sub1 m))))))

(define a<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (a< (sub1 n) (sub1 m))))))

(define a=
  (lambda (n m)
    (cond
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else (a= (sub1 n) (sub1 m))))))

(define a=2
  (lambda (n m)
    (cond
      ((a> n m) #f)
      ((a< n m) #f)
      (else #t))))

(define a^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (ax n (a^ n (sub1 m)))))))

(define a/
  (lambda (n m)
    (cond
      ((a< n m) 0)
      (else (add1 (a/ (- n m) m))))))

(define alength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (alength (cdr lat)))))))

(define apick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (apick (sub1 n) (cdr lat))))))

(define arempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (arempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((eq? a1 a2) (atom? a1))
      (else #f))))

(define eqan2?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define arempick2
  (lambda (n lat)
    (cond
      ((null? lat) '())
      ((one? n) (cdr lat))
      (else (cons (car lat) (arempick2 (sub1 n) (cdr lat)))))))
