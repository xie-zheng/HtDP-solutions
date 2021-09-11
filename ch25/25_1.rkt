;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 25_1) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (old-bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (old-bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))


; [List-of X] Number -> [List-of [List-of X]]
(define (list->chunks l n)
  (cond
    [(zero? n) (error "chunk size must > 0")]
    [(empty? l) '()]
    [else (cons (take l n) (list->chunks (drop l n) n))]))

(check-error (list->chunks '() 0))
(check-expect (list->chunks '() 3) '())
(check-expect (list->chunks '(a b c) 1) '((a) (b) (c)))
(check-expect (list->chunks '(a b c d e f g) 3) '((a b c) (d e f) (g)))


; [List-of String] Number -> [List-of String]
(define (bundle s n)
  (map implode (list->chunks s n)))

(define (partition s n)
  (cond
    [(<= (string-length s) n) (list s)]
    [else 
     (cons (substring s 0 n) (partition (substring s n) n))]))