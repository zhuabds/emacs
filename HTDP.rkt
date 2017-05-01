#lang racket

;;(require htdp/convert)

;;2.2.1
;;华氏度转摄氏度
(define (Fahrenheit->Celsius n)
  (/ (- n 32) 1.8))

;;2.2.2
;;美元转欧元
(define (dollar->euro n)
  (* n 0.9183))

;;2.2.3
;;通过底和高计算三角形面积
(define (triangle a h)
  (* (* a h) 1/2))

;;2.2.4
;;输入个十百位,输出这个数
(define (convert3 g s b)
  (+ (+ (* b 100) (* s 10)) g))

;;2.2.5
;;代数公式 n/3 +2
(define (f n)
  (+ (/ n 3) 2))

(define (f1 n)
  (+ (* n n) 10))

(define (f2 n)
  (+ (* 1/2 (* n n)) 20))

(define (f3 n)
  (- 2 (/ 1 n)))

;;2.3.1
;;计算求和
