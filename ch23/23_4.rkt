;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23_4) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; ex391
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [(empty? end) front]
    [(cons? end)
     (cons (first front) (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '(1 2) '()) '(1 2))
(check-expect (replace-eol-with '() '(1 2)) '(1 2))
(check-expect (replace-eol-with '(1 2) '(3 4)) '(1 2 3 4))