;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23_2) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length 
(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons (* (first hours) (first wages/h))
           (wages*.v2 (rest hours) (rest wages/h)))]))

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages-for hours wages/h)
  (for/list ([h hours] [w wages/h]) (* h w)))

; ex388
(define (wages* lo-employee lo-record)
  (cond
    [(empty? lo-employee) '()]
    [else
     (cons (cal-wage (first lo-employee) (first lo-record))
           (wages* (rest lo-employee)
                   (rest lo-record)))]))

(define-struct employee [name number rate])
(define-struct record [name hours])

(define (cal-wage emp reco)
  (list (employee-name emp)
        (* (employee-rate emp)
           (record-hours reco))))


; ex389
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)
(define (zip lon lop)
  (cond
    [(empty? lon) '()]
    [else (cons (make-phone-record (first lon) (first lop))
                (zip (rest lon) (rest lop)))]))