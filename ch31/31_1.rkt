;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 31_1) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define input '(50 40 70 30 30))
(define output '(50 90 160 190 220))

; [List-of Number] -> [List-of Number]
(define (measure l accumulator)
  (cond
    [(empty? l) '()]
    [else (local ((define node (+ (first l) accumulator)))
            (cons node (measure (rest l) node)))]))
(check-expect (measure input 0) output)

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))
 
; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 
(check-expect (cons 50 (add-to-each-abstract 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 
(define (add-to-each n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))
; ex489
(define (add-to-each-abstract n l)
  (map (lambda (x) (+ x n)) l))

(relative->absolute (build-list 2 add1))
