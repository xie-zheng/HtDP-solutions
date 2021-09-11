;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 22_1) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; Attributes is a list of two items:
;   (cons Symbol (cons String '()))

; ex363
; An Xexpr.v2 is a list: 
; – (cons Symbol '())
; - (cons Symbol Attributes)
; - (cons Symbol Xexpr.v2)
; Attribute is a list of two items:
;   (cons Symbol (cons String '()))
; Attributes is [List-of Attribute]

; ex364
; 1.
'(transition ((from "seen-e") (to "seen-f")))
'(ul (li (word (word))) (li (word)))

; ex365
'(server ((name "example.org")))
; <server name="example.org" />
'(carcas (board (grass)) (player ((name "sam"))))
;<carcas>
;  <board><grass /><board />
;  <player name="sam" />
;<carcas />
'(start)
; <start />


(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define content (rest xe)))
    (match content
      [(cons (? attributes?) body) (first content)]
      [body '()])))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

; Any -> Boolean
(define (attribute? x)
  (and (list? x)
       (= (length x) 2)
       (symbol? (first x))
       (string? (second x))))

; Any -> Boolean
(define (attributes? lox)
  (and (list? lox)
       (andmap attribute? lox)))
;=======================================================================

; Xexpr.v2 -> Symbol
; retrieves the name of xe
(define (xexpr-name xe)
  (first xe))

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)
;=======================================================================

; Xexpr.v2 -> [List-of Xexpr.v2]
(define (xexpr-content xe)
  (local ((define content (rest xe)))
    (match content
      [(cons (? attributes?) body) (rest content)]
      [body body])))

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
;=======================================================================

; Attributes Symbol -> Attribute
(define (find-attr attrs sym)
  (local ((define search-result (assq sym attrs)))
    (match search-result
      [#f #f]
      [(cons symbol string) (first string)])))

(check-expect (find-attr a0 'initial) "X")