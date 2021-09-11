;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9_1) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ex138

; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> PositiveNumber
; 计算总金额
(check-expect (sum (cons 2
                         (cons 1 '())))
              (+ 2 1))
(check-expect (sum '()) 0)
(check-expect (sum (cons 2 '())) 2)

(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (sum (rest l)))]))

;-----
; ex139

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; Any -> Boolean
(define (pos-num? n)
  (and (number? n)
       (> n 0)))

; List-of-numbers -> Boolean
; 判断是否都为正数
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons -1 '())) #false)

(define (pos? l)
  (cond
    [(empty? l) #true]
    [else (and (number? (first l))
               (> (first l) 0)
               (pos? (rest l)))]))

; Any -> Boolean
(define (checked-sum l)
  (cond
    [(not (pos? l)) (error "sum: PositiveNumber expected")]
    [else (sum l)]))


; ex140
; ...

; ex141
; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
  (cat (cons "ab" (cons "cd" (cons "ef" '()))))
  "abcdef")
 
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))



; ex142

; ImageOrFalse is one of:
; – Image
; – #false

; Image PositiveInteger -> Boolean
(check-expect (square-n? (rectangle 10 10 "solid" "black") 10) #true)
(check-expect (square-n? (circle 5 "solid" "black") 10) #true)
(check-expect (square-n? (rectangle 10 5 "solid" "black") 10) #false)

(define (square-n? im n)
  (and (= (image-width im) n)
       (= (image-height im) n)))



; List-of-images PositiveInteger -> ImageOrFalse
(check-expect (ill-sized? '()
                          10)
              #false)
(check-expect (ill-sized? (cons (rectangle 10 5 "solid" "black") '())
                          10)
              (rectangle 10 5 "solid" "black"))
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "black")
                                (cons (circle 5 "solid" "black") '()))
                          10)
              #false)
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "black")
                                (cons (rectangle 10 5 "solid" "black")
                                      (cons (circle 10 "solid" "black") '())))
                          10)
              (rectangle 10 5 "solid" "black"))

(define (ill-sized? l n)
  (cond
    [(empty? l) #false]
    [else (cond
            [(square-n? (first l) n) (ill-sized? (rest l) n)]
            [else (first l)])]))