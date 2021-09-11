;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 15_1) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ex250
; Number [Number -> Number] -> [List-of Number]
(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n) f))]))

(define (tab-sin n)
  (tabulate n sin))

(define (tab-sqrt n)
  (tabulate n sqrt))


; ex251
; [List-of Number] [Number Number -> Number] Number-> Number
; ...
; val0 is (f '())
(define (fold1 l f val0)
  (cond
    [(empty? l) val0]
    [else (f (first l)
             (fold1 (rest l) f val0))]))

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum1 l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum1 (rest l)))]))

(define (sum2 l)
  (fold1 l + 0))

(define l '(1 2 3 4 5 6))
(check-expect (sum1 l) (sum2 l))



; ex252
(define (fold2 l op init)
  (cond
    [(empty? l) init]
    [else (op (first l)
              (fold2 (rest l) op init))]))

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))

; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

(define (image2 l)
  (fold2 l place-dot emt))

(define l1 `(,(make-posn 1 2)
             ,(make-posn 2 3)
             ,(make-posn 3 4)))

(check-expect (image2 l1) (image* l1))