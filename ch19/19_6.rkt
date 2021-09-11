;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 19_6) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 (check-expect (substitute 'hello 'hello 'hi)
              'hi)
(check-expect (substitute '(world hello) 'hello 'hi)
              '(world hi))
(check-expect (substitute '(((world) hello) hello) 'hello 'hi)
              '(((world) hi) hi))
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
 
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))
