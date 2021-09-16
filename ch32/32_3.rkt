;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32_3) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))

(define (sum.v2 alon)
  (local ((define (sum.v2/a alon ac)
            (cond
              [(empty? alon) ac]
              [else (sum.v2/a (rest alon)
                              (+ (first alon) ac))])))
    (sum.v2/a alon 0)))

(check-expect (sum.v2 (build-list 4 add1)) 10)

; N -> N 
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v1 3) 6)
(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

(define (!.v2 n)
  (local ((define (!/a n ac)
            (cond
              [(zero? n) ac]
              [else (!/a (sub1 n) (* n ac))])))
    (!/a n 1)))

(check-expect (!.v2 4) 24)


(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

(define (height.v2 abt)
  (local ((define (height/a abt accu)
            (cond
              [(empty? abt) 0]
              [else (max (height/a (node-left abt) (add1 accu))
                         (height/a (node-right abt) (add1 accu)))])))
    (height/a abt 0)))

; ex499
(define (product l)
  (local ((define (product/a l ac)
            (cond
              [(empty? l) ac]
              [else (product/a (rest l) (* (first l) ac))])))
    (product/a l 1)))

; ex501
; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (local ((define (add-to-pi/a n ac)
            (cond
              [(zero? n) ac]
              [else (add-to-pi/a (sub1 n) (add1 ac))])))
    (add-to-pi/a n pi)))

; ex502//important
(check-expect (palindrome (explode "abc")) (explode "abcba"))
(define (palindrome l)
  (local ((define (palindrome/a l ac)
            (cond
              [(empty? (rest l)) (cons (first l) ac)]
              [else (cons (first l) (palindrome/a (rest l) (cons (first l) ac)))])))
    (palindrome/a l '())))

; ex503
; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(define (rotate M)
  (local ((define (rotate/a M seen)
            (cond
              [(not (= (first (first M)) 0)) M]
              [(member? (first M) seen) (error "all pivots are 0")]
              [else (rotate/a (append (rest M) (list (first M))) (cons (first M) seen))])))
    (rotate/a M '())))

(define (rotate.v2 M0)
  (local (; Matrix ... -> Matrix 
          ; accumulator ...
          (define (rotate/a M seen)
            (cond
              [(empty? (rest M)) (append M seen)] ; Can this be simplified to (empty? M)
              [else (if (= (first (first M)) 0)
                        (rotate/a (rest M)
                                  (cons (first M) seen))
                        (append M seen))])))
    (rotate/a M0 '())))

; ex504
(define (to10 l)
  (local ((define (to10/a l ac)
            (cond
              [(empty? l) ac]
              [else (to10/a (rest l) (+ (first l) (* ac 10)))])))
    (to10/a l 0)))
(check-expect (to10 '(1 0 2)) 102)

; ex506
(define (map/ f l)
  (local ((define (map/a f l ac)
            (cond
              [(empty? l) ac]
              [else (map/a f (rest l) (cons (f (first l)) ac))])))
    (map/a f l '())))

; ex507
(define (build-l*st n f)
  (local ((define (build-l*st/a n f accumulator)
            (local ((define ac (cons (f n) accumulator)))
              (cond
                [(zero? n) ac]
                [else (build-l*st/a (sub1 n) f ac)]))))
    (build-l*st/a (sub1 n) f '())))