;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 21_3) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define WRONG " =====================\n|Something goes wrong.|\n =====================")


(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [sym arg])


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
(define (eval-add x arg-evaler)
  (+ (arg-evaler (add-left x))
     (arg-evaler (add-right x))))

; mul -> Nubmer
(define (eval-mul x arg-evaler)
  (* (arg-evaler (mul-left x))
     (arg-evaler (mul-right x))))
;=======================================================================

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-func Symbol BSL-fun-expr)

; BSL-fun-expr Symbol Number -> BSL-fun-expr
(define (subst ex x v)
  (match ex
    [(? number?) ex]
    [(? symbol?) (if (equal? ex x) v ex)]
    [(add l r) (make-add (subst l x v) (subst r x v))]
    [(mul l r) (make-mul (subst l x v) (subst r x v))]
    [(fun s a) (make-fun s (subst a x v))]))

(check-expect (subst (make-add 'x 1) 'x 3) (make-add 3 1))
(check-expect (subst (make-add 'x 1) 'y 3) (make-add 'x 1))


; BSL-fun-expr -> Boolean
(define (numeric? ex)
  (match ex
    [(? number?) #t]
    [(? symbol?) #f]
    [(add l r) (and (numeric? l) (numeric? r))]
    [(mul l r) (and (numeric? l) (numeric? r))]))
(check-expect (numeric? (make-add 'x 1)) #f)
(check-expect (numeric? (make-mul 1 2)) #t)


; BSL-fun-expr -> Number
(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error "expect: numeric expression\n-> received: non numeric")))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; BSL-fun-expr AL -> Number
(define (eval-variable* ex al)
  (eval-variable
   (foldr
    (lambda (assoc pre) (subst pre (first assoc) (second assoc)))
    ex
    al)))

; BSL-fun-expr AL -> Number
(define (eval-var-lookup e da)
  (eval-variable (subst-all e da)))

; BSL-fun-expr AL -> BSL-fun-expr
(define (subst-all e da)
  (match e
    [(? number?) e]
    [(? symbol?) (eval-symbol e da)]
    [(add l r) (make-add (subst-all l da) (subst-all r da))]
    [(mul l r) (make-mul (subst-all l da) (subst-all r da))]))

; Symbol AL -> ...
(define (eval-symbol sym da)
  (local ((define search-result (assq sym da)))
    (if (equal? #f search-result)
        sym ; remain unchanged
        (second search-result))))

;=======================================================================
; ex357
; BSL-fun-expr Symbol Symbol BSL-fun-expr
(define (eval-definition1 ex f x b)
  (match ex
    [(? symbol?) (error WRONG)]
    [(? number?) ex]
    [(? add?) (eval-add1 ex f x b)]
    [(? mul?) (eval-mul1 ex f x b)]
    [(? fun?) (eval-func ex f x b)]))

(define (eval-mul1 ex f x b)
  (* (eval-definition1 (mul-left ex) f x b)
     (eval-definition1 (mul-right ex) f x b)))

(define (eval-add1 ex f x b)
  (+ (eval-definition1 (add-left ex) f x b)
     (eval-definition1 (add-right ex) f x b)))

(define (eval-func ex f x b)
  (local ((define value (eval-definition1 (fun-arg ex) f x b))
          (define plugd (subst b x value)))
    (eval-definition1 plugd f x b)))


; ex358
; A BSL-fun-def is a struct:
;   (make-func Symbol [List-of Symbol] [BSL-fun-expr])
(define-struct func [name para body])
(define f (make-func 'f 'x (make-add 3 'x)))
(define g (make-func 'g 'y (make-fun 'f (make-mul 2 'y))))
(define h (make-func 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; A BSL-fun-def* is [List-of BSL-fun-def]
(define da-fgh
  (list f g h))


; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(define (lookup-def da f)
  (match da
    [(? empty?) (error "not find")]
    [(cons fst rst) (if (equal? (func-name fst) f)
                        fst
                        (lookup-def rst f))]))


; ex259
; BSL-fun-expr, BSL-fun-def*
(define (eval-function* ex da)
  (match ex
    [(? number?) ex]
    [(? symbol?) (error WRONG)]
    [(add l r) (+ (eval-function* l da) (eval-function* r da))]
    [(mul l r) (* (eval-function* l da) (eval-function* r da))]
    [(fun f x) (local ((define function (lookup-def da f))
                       (define value (eval-function* x da))
                       (define expr (subst (func-body function)
                                           (func-para function)
                                           value)))
                 (eval-function* expr da))]))













