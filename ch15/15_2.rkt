;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 15_2) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ex254
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
(define (sort-n l compare)
  (cond
    [(empty? (rest l)) l]
    [else (sort-insert (first l)
                       (sort-n (rest l) compare)
                       compare)]))

; Number [List-of Number] [Number Number -> Boolean] -> [List-of Number]
(define (sort-insert n l compare)
  (cond
    [(empty? l) (list n)]
    [else (if (compare n (first l))
              (cons n l)
              (cons (first l)
                    (sort-insert n
                                 (rest l)
                                 compare)))]))

; Abstract
; [X] [List-of X] [X X -> Boolean] -> [List-of X]

; ex255
; [X] [List-of X] [X -> X] -> [List-of X]