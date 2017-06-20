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

(define eqan2?
  (lambda (a1 a2)
    (cond
      ((eq? a1 a2) (atom? a1))
      (else #f))))

(define eqan?
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


;;;05 Oh My Gawd: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member2*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l))
           (member* a (cdr l))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))

(define leftmost2
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist2?
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) (null? lat2))
      ((null? lat2) (null? lat1))
      ((and (atom? (car lat1)) (atom? (car lat2)))
       (and (eqan? (car lat1) (car lat2))
            (eqlist? (cdr lat1) (cdr lat2))))
      (else
       (and (eqlist? (car lat1) (car lat2))
            (eqlist? (cdr lat1) (cdr lat2)))))))

(define eqlist3?
  (lambda (lat1 lat2)
    (cond
      ((and (null? lat1) (null? lat2)) #t)
      ((or (null? lat1) (null? lat2)) #f)
      ((and (atom? (car lat1)) (atom? (car lat2)))
       (and (eqan? (car lat1) (car lat2))
            (eqlist? (cdr lat1) (cdr lat2))))
      (else
       (and (eqlist? (car lat1) (car lat2))
            (eqlist? (cdr lat1) (cdr lat2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else
       (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

;;;06 Shadows

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+)
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '*)
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^)
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) '+)
       (+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '*)
       (* (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^)
       (a^ (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

(define value2
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car aexp) '+)
       (+ (value2 (car (cdr aexp))) (value2 (car (cdr (cdr aexp))))))
      ((eq? (car aexp) '*)
       (* (value2 (car (cdr aexp))) (value2 (car (cdr (cdr aexp))))))
      (else
       (a^ (value2 (car (cdr aexp))) (value2 (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2st-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value3
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) '+)
       (+ (value3 (1st-sub-exp aexp)) (value3 (2st-sub-exp aexp))))
      ((eq? (operator aexp) '*)
       (* (value3 (1st-sub-exp aexp)) (value3 (2st-sub-exp aexp))))
      (else
       (a^ (value3 (1st-sub-exp aexp)) (value3 (2st-sub-exp aexp)))))))
      
(define sero?
  (lambda (n)
    (cond
      ((atom? n) #f)
      (else (null? n)))))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define e+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
       (edd1 (e+ n (zub1 m)))))))


;;;07 Firends an Relations

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
      (else
       (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset2 (cdr lat)))
      (else
       (cons (car lat) (makeset2 (cdr lat)))))))

(define makeset3
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat) (makeset3 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2) (subset2? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
    ((null? set1) '#f)
    (else
     (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      (else
       (makeset3 (cons (car set1) (union (cdr set1) set2)))))))

(define union2
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union2 (cdr set1) set2))
      (else
       (cons (car set1) set2)))))

(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (xxx (cdr set1) set2))
      (else
       (cons (car set1) (xxx (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
       (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      (else
       (null? (cdr (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel2
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons
        (cons
         (car (cdr (car rel)))
         (cons
          (car (car rel))
          '()))
        (revrel2 (cdr rel)))))))

(define afirst
  (lambda (s)
    (car s)))

(define asecond
  (lambda (s)
    (car (cdr s))))

(define abuild
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (abuild
              (asecond (car rel))
              (afirst (car rel)))
             (revrel (cdr rel)))))))

(define revpair
  (lambda (s)
    (abuild (asecond s) (afirst s))))

(define revrel3
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel))
             (revrel3 (cdr rel)))))))

(define fullfun2?
  (lambda (rel)
    (fun? (revrel3 rel))))

(define seconds
  (lambda (s)
    (cond
      ((null? s) '())
      (else
       (cons (car (cdr (car s))) (seconds (cdr s)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))          

;; (define cookies
;;   (lambda ()
;;     (bake
;;      (quote (350 degrees))
;;      (quote (12 minutes))
;;      (mix
;;       (quote (walnuts 1 cup))
;;       (quote (chocolate-chips 16 ounces))
;;       (mix
;;        (mix
;;         (quote (flour 2 cups))
;;         (quote (oatmeal 2 cups))
;;         (quote (salt 5 teaspoon))
;;         (quote (baking-powder 1 teaspoon))
;;         (quote (baking-soda 1  teaspoon)))
;;        (mix
;;         (quote (eggs 2 large))
;;         (quote (vanilla 1 teaspoon))
;;         (cream
;;          (quote (butter 1 cup))
;;          (quote (sugar 2 cups)))))))))


;;; 08 Lambda the Ultimate

(define rember-f
  (λ (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (rember-f test? a (cdr l)))
      (else
       (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (λ (a)
    (λ (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(define rember-f2
  (λ (test?)
    (λ (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) ((rember-f2 test?) a (cdr l)))
        (else
         (cons (car l) ((rember-f2 test?) a (cdr l))))))))

(define rember-eq?
  (rember-f2 eq?))

(define insertL-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l))
         (cons new (cons old (cdr l))))
        (else
         (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l))
         (cons old (cons new (cdr l))))
        (else
         (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (λ (new old l)
    (cons new (cons old l))))

(define seqR
  (λ (new old l)
    (cons old (cons new l))))

(define insert-g
  (λ (test? seqLR)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (seqLR new old (cdr l)))
        (else
         (cons (car l) ((insert-g test? seqLR) new old (cdr l))))))))

(define insertL-f2
  (insert-g eq?
            (λ (new old l)
              (cons new (cons old l)))))

(define insertR-f2
  (insert-g eq?
            (λ (new old l)
              (cons old (cons new l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-f
  (insert-g eq?
            (lambda (new old l)
              (cons new l))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else a^))))

(define value-f
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value-f (1st-sub-exp nexp))
        (value-f (2st-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l))
         ((multirember-f test?) a (cdr l)))
        (else
         (cons (car l) ((multirember-f test?) a (cdr l))))))))

(define multirember-eq?
  (multirember-f eq?))

(define atest?
  (lambda (a)
    (lambda (b)
      (eq? a b))))

(define eq?-a
  (atest? 'a))

(define multiremberT
  (lambda (test? l)
    (cond
      ((null? l) '())
      ((test? (car l))
       (multiremberT test? (cdr l)))
      (else
       (cons (car l) (multiremberT test? (cdr l)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? a (car lat))
       (multirember&co a (cdr lat)
                       (lambda (al bl)
                         (col al (cons (car lat) bl)))))
      (else
       (multirember&co a (cdr lat)
                       (lambda (al bl)
                         (col (cons (car lat) al) bl)))))))

(define a-friend
  (lambda (al bl)
    (null? bl)))

(define new-friend
  (lambda (al bl)
    (cons al (cons '列表分割 bl))))

(define last-friend
  (lambda (al bl)
    (length al)))


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? oldL (car lat))
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat))
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define coln
  (lambda (newlat L R)
    (cons L (cons R newlat))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (add1 L) R))))
      ((eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat) L R)))))))

(define even?
  (lambda (n)
    (cond
      ((= n 1) #f)
      ((= n 2) #t)
      (else (even? (- n 2))))))

(define evens-only*
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((even? (car lat))
          (cons (car lat) (evens-only* (cdr lat))))
         (else
          (evens-only* (cdr lat)))))
      (else
       (cons (evens-only* (car lat)) (evens-only* (cdr lat)))))))

(define even-col
  (λ (newlat a b)
    (cons a (cons b newlat))))

(define evens-only*&co
  (lambda (lat col)
    (cond
      ((null? lat) (col '() 0 1))
      ((atom? (car lat))
       (cond
         ((even? (car lat))
          (evens-only*&co (cdr lat)
                          (lambda (newlat a b)
                            (col (cons (car lat) newlat)
                                 a
                                 (* (car lat) b)))))
         (else
          (evens-only*&co (cdr lat)
                          (lambda (newlat a b)
                            (col newlat
                                 (+ (car lat) a)
                                 b))))))
      (else
       (evens-only*&co (car lat)
                       (lambda (al ap as)
                         (evens-only*&co (cdr lat)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (+ ap dp)
                                                (* as ds))))))))))


;; ;;;09 ...and Again, and Again, and Again,...

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) '())
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))

(define keep-looking
  (lambda (a b lat)
    (cond
      ((number? b)
       (cond
         ((eq? a (pick b lat)) #t)
         (else (keep-looking a (pick b lat) lat))))
      (else (eq? a b)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define build
  (lambda (a b)
    (cons a (cons b '()))))

(define shift
  (lambda (x)
    (build (first (first x))
           (build (second (first x))
                  (second x)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (length* (first pora))
          (length* (second pora)))))))

(define weight*
  (lambda (x)
    (cond
      ((atom? x) 1)
      (else
       (+ (* (weight* (first x)) 2)
          (weight* (second x)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

;;;Y算子部分

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(define y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;;;通过Y算子理解什么是递归 没有看明白

;;;开始第十章

;;;10 What is the Values of All of This?

(define new-entry build)

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else
       (lookup-in-entry name (cdr names) (cdr values) entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define extend-table
  cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry name
                        (car table)
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table)
                                           table-f)))))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define text-of
  second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define cond-line-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-line-of e) table)))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #t)
      ((eq? (car x) (quote primitive)) #t)
      (else #f))))


(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure) vals)
              (table-of closure)))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) apply-closure (second fun) vals))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))


(define valueA
  (lambda (e)
    (meaning e (quote ()))))


;; (define pick
;;   (lambda (n lat)
;;     (cond
;;       ((null? lat) null)
;;       ((eq? (sub1 n) 0) (car lat))
;;       (else
;;        (pick (sub1 n) (cdr lat))))))

;; (define keep-looking
;;   (lambda (a l lat)
;;     (cond
;;       ((number? l) (keep-looking a (pick l lat) lat))
;;       (else (eq? a l)))))

;; (define build
;;   (lambda (a b)
;;     (cons a (cons b '()))))

;; (define shift
;;   (lambda (x)
;;     (build (first (first x))
;;            (build (second (first x))
;;                   (second x)))))

;; (define align
;;   (lambda (pora)
;;     (cond
;;       ((atom? pora) pora)
;;       ((a-pair? (first pora)) (align (shift pora)))
;;       (else (build (first pora) (align (shift pora)))))))

;; (define length*
;;   (lambda (pora)
;;     (cond
;;       ((null? pora) 0)
;;       ((atom? pora) 1)
;;       (else
;;        (+ (length* (car pora)) (length* (cdr pora)))))))
