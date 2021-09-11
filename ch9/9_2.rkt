;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9_2) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; List-of-temperatures -> Number
; computes the average temperature
(check-expect
  (average (cons 1 (cons 2 (cons 3 '())))) 2)

; List-of-temperatures -> Number
; computes the average temperature 
(define (average alot)
  (/ (sum alot) (how-many alot)))

(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)
; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

 
; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ 1 (how-many (rest alot)))]))

; ex143
(check-error (checked-average '()))
(check-expect (checked-average (cons 1 (cons 2 (cons 3 '()))))
              (average (cons 1 (cons 2 (cons 3 '())))))
(define (checked-average alot)
  (cond
    [(empty? alot) (error "empty list")]
    [else (average alot)]))







; ex145
; NEList-of-temperatures -> Bool
(define (sorted? ne-l)
  (or (asc? ne-l) (dec? ne-l)))
(check-expect (sorted? (cons 3 (cons 1 (cons 2 '()))))
              #false)
(check-expect (sorted? (cons 1 (cons 2 (cons 3 '()))))
              #true)

(define (asc? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (and (< (first ne-l) (first (rest ne-l)))
               (asc? (rest ne-l)))]))

(define (dec? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (and (> (first ne-l) (first (rest ne-l)))
               (dec? (rest ne-l)))]))
