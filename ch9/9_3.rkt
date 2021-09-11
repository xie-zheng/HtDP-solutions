;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9_3) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; N String -> List-of-strings 
; creates a list of n copies of s
 
(check-expect (copier 0 "hello") '())
(check-expect (copier 2 "hello")
              (cons "hello" (cons "hello" '())))
 
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))

(define (copier.v2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier.v2 (sub1 n) s))]))

;(copier 0.1 "x")

; ex150
; N -> Number
; computes (+ n pi) without using +

(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; N, Number -> Number
; computes (+ n x) without using +

(check-within (add 3 5.5) (+ 3 5.5) 0.001)

(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n) (add1 (add (sub1 n) x))]))

; ex151
; N, Number -> Number
; computes (* n x) without using *

(check-within (multiply 3 5.5) (* 3 5.5) 0.01)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(positive? n) (+ x (multiply (sub1 n) x))]))

; ex152
; N, Image -> Image
; produces a column—a vertical arrangement—of n copies of img.
(define (col n img)
  (cond
    [(= n 1) img]
    [(positive? n) (above img (col (sub1 n) img))]))

; N, Image -> Image
; produces a row vertical arrangement—of n copies of img.
(define (row n img)
  (cond
    [(= n 1) img]
    [(positive? n) (beside img (row (sub1 n) img))]))

(define box (rectangle 10 10 "outline" "black"))

; ex153
; lecture hall
(define BOX (rectangle 10 10 "outline" "black"))
(define RECT (row 10 (col 20 BOX)))
(define HALL (place-image RECT
                          (/ (image-width RECT) 2)
                          (/ (image-height RECT) 2)
                          (empty-scene (image-width RECT) (image-height RECT))))
(define DOT (circle 2 "solid" "red"))

; List of Posn, Image -> Image
; THe function consumes a list of Posn whose coordinates
;   fit into the dimensions of the lecture hall.
;   It produces an image of the lecture hall with red dots
;   added as specified by the Posns.
(define (add-ballons posns hall)
  (cond
    [(empty? posns) hall]
    [else (add-ballons (rest posns)
                       (add-ballon (first posns) hall))]))

; Posn, Image -> Image
; produces an image of the leture hall with red dots
; added as specified by the Posn
(define (add-ballon posn hall)
  (place-image DOT
               (posn-x posn)
               (posn-y posn)
               hall))

(define attacks
  (list (make-posn 10 20)
        (make-posn 32 80)
        (make-posn 99 20)))
