;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex105) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

(define cor1 10)
(define cor2 (make-posn 10 10))

(define dot (circle 3 "solid" "red"))
(define BG (empty-scene 100 100))
; Coordinate -> Image
; render the position of the 
(define (render cor)
  (cond
    [(posn? cor) (place-image dot
                              (posn-x cor)
                              (posn-y cor)
                              BG)]
    [(number? cor) (cond
                     [(>= cor 0) (place-image dot cor 0 BG)]
                     [(< cor 0) (place-image 0 cor BG)])]))

(render cor1)
(render cor2)
                              