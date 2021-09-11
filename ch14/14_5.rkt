;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 14_5) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp")) #f)))
(define (f x) x)
(cons f '())
(f f)
(cons f (cons 10 (cons (f 10) '())))
