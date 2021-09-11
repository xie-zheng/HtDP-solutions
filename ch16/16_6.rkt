;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 16_6) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; [List-of Posn] -> Image 
; adds the Posns on lop to the empty scene 
 
(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 MT-SCENE))
 
(define (dots lop)
  MT-SCENE)

