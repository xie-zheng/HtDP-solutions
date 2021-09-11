;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 14_3) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; ex239
; A [List X Y] is a list: 
;   (cons X (cons Y '()))

; (1)
; [List [List-of Number] [List-of Number]]
; (2)
; [List [List-of Number] [List-of 1String]]
; ...

; An [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NElist-of ITEM])

; A [Maybe [List-of String]] is one of:
; - #false
; - [List-of String]
; i.e.
; - #false
; - (cons String '())
; - (cons String [List-of String])
; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #f]
    [else (if (string=? (first los) s)
              (rest los)
          (occurs s (rest los)))]))