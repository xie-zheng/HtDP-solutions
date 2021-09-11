;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 10_1) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))

(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '()))
              (cons 392 '()))
(check-expect (wage* (cons 4 (cons 2 '())))
              (cons 56 (cons 28 '())))
              
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* 14 h))

; Number -> Number
(define (check h)
  (cond
    [(> h 100) (error "impossible to work more than 100 hours!")]
    [else h]))

(define (checked-wage h)
  (wage (check h)))




  



