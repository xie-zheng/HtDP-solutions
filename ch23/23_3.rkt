;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23_3) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (list-pick los n)
  (cond
    [(empty? los) (error "list too short")]
    [(= n 0) (first los)]
    [(> n 0) (list-pick (rest los) (sub1 n))]))

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")

; ex390
(define-struct branch [left right])

; Branch [List-of Direction] -> TOS
(define (tree-pick tree lod)
  (cond
    [(empty? lod) tree]
    [(symbol? tree) (error "tree too short")]
    [else
     (local ((define direction (first lod))
             (define next (if (equal? direction 'left)
                              (branch-left tree)
                              (branch-right tree))))
       (tree-pick next (rest lod)))]))

