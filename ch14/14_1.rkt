;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 14_1) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; =======ex235============
; Los -> Boolean
(define (contains-atom? l)
  (contains? "atom" l))

; "basic" "zoo" etc.





; =======ex236============
; Lon -> Lon
; adds 1 to each item on l
(define (add1* l)
  (add* 1 l))
(check-expect (add1* '(1 2 3 4 5)) '(2 3 4 5 6))

	
; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (add* 5 l))
(check-expect (plus5 '(1 2 3 4 5)) '(6 7 8 9 10))

; Lon Number -> Lon
; adds n to each item on l
(define (add* n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ (first l) n)
      (add* n (rest l)))]))