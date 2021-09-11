;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
(define angle 100)
(define color "green")
(define transparency 200)
(define first-width 20)
(define (ith-triangle i)
  (isosceles-triangle (+ first-width (* i 10)) angle transparency color))

(define leaves
  (overlay/align/offset 

(overlay/align/offset "middle" "middle"
 (isosceles-triangle 100 100 100 "green")
 0
 20
 (isosceles-triangle 100 100 100 "green"))