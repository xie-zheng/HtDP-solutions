;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 18_3) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol

; ??? -> Boolean
(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count-old sexp sy)
  (cond
    [(atom? sexp) (count-atom sexp sy)]
    [else (count-sl sexp sy)]))
 
; SL Symbol -> N 
; counts all occurrences of sy in sl 
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+ (count-old (first sl) sy) (count-sl (rest sl) sy))]))
 
; Atom Symbol -> N 
; counts all occurrences of sy in at 
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)






;=======================================================================
; ex317
; S-expr Symbol -> N
(define (count sexp sy)
  (local (; SL -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))

          ; Atom Symbol -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    ; - IN -
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))



; ex318
; S-expr -> N
(define (depth sexp)
  (cond
    [(atom? sexp) 1]
    [else (depth-sl sexp)]))

(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)

(define (depth-sl sl)
  (+ (foldr max 1 (map depth sl)) 1))



; ex319
; S-expr Symbol Symbol -> S-expr
; ...
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (substitute-atom sexp old new)]
    [else (substitute-sl sexp old new)]))

; Atom Symbol Symbol -> Atom
(define (substitute-atom at old new)
  (cond
    [(number? at) at]
    [(string? at) at]
    [(symbol? at) (if (symbol=? at old) new at)]))

; SL Symbol Symbol -> SL
(define (substitute-sl sl old new)
  (cond
    [(empty? sl) sl]
    [else (cons (substitute (first sl) old new)
                (substitute-sl (rest sl) old new))]))

(check-expect (substitute 'hello 'hello 'hi)
              'hi)
(check-expect (substitute '(world hello) 'hello 'hi)
              '(world hi))
(check-expect (substitute '(((world) hello) hello) 'hello 'hi)
              '(((world) hi) hi))



; ex320
; An S-expr is one of: 
; – Number
; - String
; - Symbol
; – [List-of S-expr]

; S-expr Symblo -> N
(define (new-count sexp sy)
  (cond
    [(list? sexp) (foldr + 0 (map (lambda (x) (new-count x sy)) sexp))]
    [(symbol? sexp) (if (equal? sexp sy) 1 0)]
    [else 0]))
(check-expect (new-count 'world 'hello) 0)
(check-expect (new-count '(world hello) 'hello) 1)
(check-expect (new-count '(((world) hello) hello) 'hello) 2)