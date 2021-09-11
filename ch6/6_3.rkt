;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 6_3) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ex110

; Number -> Number
; computes the area of a disk with radius r
(define (area-of-disk r)
  (* 3.14 (* r r)))

; Any -> Number
; computes the area of a disk with radius v, 
; if v is a number
(define (checked-area-of-disk v)
  (cond
    [(number? v) (if (> v 0) (area-of-disk v)
                     (error "area-of-disk: positive number expected"))]
    [else (error "area-of-disk: number expected")]))

; ---
; ex111

(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

; Any -> Boolean
(define (positive-number? n)
  (if (and (number? n) (> n 0)) #true #false))

; Any Any -> Error
(define (produce-error x y)
  (error
   (string-append "make-vec:\n"
                  (cond
                    [(number? x) (if (> x 0) "" "[x] positive number expect\n")]
                    [else "[x] number expect\n"])
                  (cond
                    [(number? y) (if (> y 0) "" "[y] positive number expect\n")]
                    [else "[y] number expect\n"]))))

; Any Any -> vec
(define (checked-make-vec x y)
  (if (and (positive-number? x) (positive-number? y))
      (make-vec x y)
      (produce-error x y)))


; Any Any -> vec
(define (checked-make-vec.v2 x y)
  (cond
    [(and (number? x) (number? y))
     (if (or (<= x 0) (<= y 0))
         (error "make-vec: positive number expect")
         (make-vec x y))]
    [else (error "make-vec: number expect")]))



; ex112
(define (missile-or-not? v)
  (or (false? v)
      (posn? v)))

; ex113
; ...

; ex114
