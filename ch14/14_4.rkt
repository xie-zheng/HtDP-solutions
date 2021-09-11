;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 14_4) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp")) #f)))
(define (f x) (x f))
(define (a n) (n a))

; ex245
(define (function=at-1.2-3-and-5.775? f1 f2)
  (and
   (function=? f1 f2 1.2)
   (function=? f1 f2 3)
   (function=? f1 f2 -5.775)))

(define (function=? f1 f2 n)
  (equal? (f1 n) (f2 n)))