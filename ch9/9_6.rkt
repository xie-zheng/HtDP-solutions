;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9_6) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; List-of-string String -> N
; determines how often s occurs in los
(define (count los s)
  (cond
    [(empty? los) 0]
    [(string=? s (first los)) (add1 (count (rest los) s))]
    [else (count (rest los) s)]))





; Son
(define es '())
 
; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

; Number Son.L -> Son.L
; removes x from s 
(define s1.L
  (cons 1 (cons 1 '())))
 
(check-expect
  (set-.L 1 s1.L) es)
 
(define (set-.L x s)
  (remove-all x s))
  

; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))
 
(check-expect
  (set-.R 1 s1.R) es)
 
(define (set-.R x s)
  (remove x s))

; Number Son -> Son
; subtracts x from s 
(define (set- x s)
  (cond
    [(empty? s) empty]
    [else (cond
            [(= x (first s)) (set- x (rest s))]
            [else (cons (first s) (set- x (rest s)))])]))


(define set123-version1
  (cons 1 (cons 2 (cons 3 '()))))
 
(define set123-version2
  (cons 1 (cons 3 (cons 2 '()))))

(define set23-version1
  (cons 2 (cons 3 '())))

; Son -> Boolean
; #true if 1 is not a member of s;  #false otherwise
(define (not-member-1? s)
  (not (in? 1 s)))

(check-satisfied (set- 1 set123-version1) not-member-1?)
 
(define set23-version2
  (cons 3 (cons 2 '())))

(check-member-of (set-.v1 1 set123.v1)
                 set23-version1
                 set23-version2)

; ex160
(define (set+.L x s)
  (cons x s))

(define (set+.R x s)
  (cond
    [(in? x s) s]
    [else (cons x s)]))

(define (map func list)
  (cond
    [(empty? list) '()]
    [else (cons (func (first list))
                (map func (rest list)))]))