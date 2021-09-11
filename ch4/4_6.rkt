;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 4_6) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))

; A Price falls into one of three intervals: 
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item 
(define NOT-TAXED 1000)
(define LUXURY 10000)
(define NORMAL-RATE 0.05)
(define LUXURY-RATE 0.08)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* NORMAL-RATE 1000))
(check-expect (sales-tax 1282) (* NORMAL-RATE 1282))
(check-expect (sales-tax 12017) (* LUXURY-RATE 12017))

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p NOT-TAXED)) 0]
    [(and (<= 1000 p) (< p LUXURY)) (* NORMAL-RATE p)]
    [(>= p LUXURY) (* LUXURY-RATE p)]))
