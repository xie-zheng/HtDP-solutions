;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 21_2) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct add [left right])
(define-struct mul [left right])

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

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr Symbol Number -> BSL-var-expr
(define (subst ex x v)
  (match ex
    [(? number?) ex]
    [(? symbol?) (if (equal? ex x) v ex)]
    [(add l r) (make-add (subst l x v) (subst r x v))]
    [(mul l r) (make-mul (subst l x v) (subst r x v))]))

(check-expect (subst (make-add 'x 1) 'x 3) (make-add 3 1))
(check-expect (subst (make-add 'x 1) 'y 3) (make-add 'x 1))


; BSL-var-expr -> Boolean
(define (numeric? ex)
  (match ex
    [(? number?) #t]
    [(? symbol?) #f]
    [(add l r) (and (numeric? l) (numeric? r))]
    [(mul l r) (and (numeric? l) (numeric? r))]))
(check-expect (numeric? (make-add 'x 1)) #f)
(check-expect (numeric? (make-mul 1 2)) #t)


; BSL-var-expr -> Number
(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error "expect: numeric expression\n-> received: non numeric")))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; BSL-var-expr AL -> Number
(define (eval-variable* ex al)
  (eval-variable
   (foldr
    (lambda (assoc pre) (subst pre (first assoc) (second assoc)))
    ex
    al)))

; BSL-var-expr AL -> Number
(define (eval-var-lookup e da)
  (eval-variable (subst-all e da)))

; BSL-var-expr AL -> BSL-var-expr
(define (subst-all e da)
  (match e
    [(? number?) e]
    [(? symbol?) (eval-symbol e da)]
    [(add l r) (make-add (subst-all l da) (subst-all r da))]
    [(mul l r) (make-mul (subst-all l da) (subst-all r da))]))

; Symbol AL -> ...
(define (eval-symbol sym da)
  (local (; search symbol in da
          (define search-result (assq sym da)))
    ; - IN -
    (if (equal? #f search-result)
        sym
        (second search-result))))
         