;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 21_1) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct add [left right])
(define-struct mul [left right])

(make-add 1 1)
(make-mul 3 10)
(make-add (make-mul 1 1) 10)

; BSL-expr -> Number
(define (eval-expression expr)
  (cond
    [(number? expr) (eval-number expr)]
    [(add? expr) (eval-add expr)]
    [(mul? expr) (eval-mul expr)]))

; Number -> Number
(define (eval-number x)
  x)

; add -> Number
(define (eval-add x)
  (+ (eval-expression (add-left x))
     (eval-expression (add-right x))))

; mul -> Nubmer
(define (eval-mul x)
  (* (eval-expression (mul-left x))
     (eval-expression (mul-right x))))
;=======================================================================

(define-struct bool [op left right])

; bool -> Boolean
(define (eval-bool-expression expr)
  (cond
    [(boolean? expr) expr]
    [(bool? expr) (match expr
                    [(bool 'and l r) (and (eval-bool-expression l)
                                          (eval-bool-expression r))]
                     [(bool 'or l r) (or (eval-bool-expression l)
                                          (eval-bool-expression r))]
                     [(bool 'not l r) (not (eval-bool-expression l))])]))

;=======================================================================
; S-expr -> Boolean
(define (atom? s)
  (or (number? s)
      (string? s)
      (symbol? s)))

;
(define WRONG "Something goes wrong")

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(+ (* 1 1) 10)) (make-add (make-mul 1 1) 10))
(check-error (parse '(and #t #t)))

(define (bsl? s)
  (or (add? s)
      (mul? s)))
;
(define (interpreter-expr s)
  ((lambda (expr) (if (bsl? expr) (eval-expression expr) (error WRONG)))
   (parse s)))
;=======================================================================