;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 21_4) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define WRONG
  " =====================\n|Something goes wrong.|\n =====================")

(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [sym arg])
(define-struct func [name para body])

; BSL-da-all Symbol -> B-expr
(define (lookup da x)
  (local ((define search-result (assq x da)))
    (if (equal? #f search-result)
        (error (string-append (symbol->string x) " not defined"))
        (second search-result))))

; B-expr BSL-da-all -> ...
(define (eval-all expr da)
  (match expr
    [(? number?) expr]
    [(? symbol?) (eval-all (lookup da expr) da)]
    [(add l r) (+ (eval-all l da) (eval-all r da))]
    [(mul l r) (* (eval-all l da) (eval-all r da))]
    [(fun f x) (apply-to f x da)]))

; Symbol B-expr BSL-da-all 
(define (apply-to f x da)
  (local ((define function (lookup da f))
          (define value (eval-all x da))
          (define expr (subst (func-body function) (func-para function) value)))
    (eval-all expr da)))

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



(define area
  (list
   (list 'a 10)
   (list 'b (make-mul 2 'a))
   (list 'f (make-func 'f 'x (make-mul 'x 2)))
   (list 'g (make-func 'g 'x (make-add 'x 3)))))

(check-expect (eval-all (make-fun 'f 'a) area) (eval-all 'b area))




;=======================================================================
; PARSER
; ------
; S-expr -> BSL-expr
(define (parse s sl)
  (local ((define env (parse-env sl))
          (define exprs (map parse-expr s)))
    (map (lambda (x) (eval-all x env)) exprs)))

; SL -> BSL-da-all
(define (parse-env sl)
  (map parse-define sl))

; S-expr -> BSL-da
(define (parse-define s)
  (match s
    [(list 'define (list f x) body) ; function define
     (list f (make-func f x (parse-expr body)))]
    [(list 'define name value)      ; constant define
     (list name (parse-expr value))]
    [expr (error WRONG)]))


; SL -> BSL-expr 
(define (parse-expr s)
  (cond
    [(atom? s) (parse-atom s)]
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse-expr (second s)) (parse-expr (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse-expr (second s)) (parse-expr (third s)))]
       [else (error WRONG)])]
    [(and (consists-of-2 s) (symbol? (first s)))
     (make-fun (first s) (parse-expr (second s)))]
    [else (error WRONG)]))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(symbol? s) s]
    [(string? s) (error WRONG)]))

; S-expr -> Boolean
(define (atom? s)
  (or (number? s)
      (symbol? s)
      (string? s)))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; SL -> Boolean
(define (consists-of-2 s)
  (and (cons? s) (cons? (rest s))
       (empty? (rest (rest s)))))


(define defines
  '((define a 10)
    (define b (* 2 a))
    (define (f x) (* x 2))
    (define (g x) (+ x 3))))

(define exprs
  '((+ a 10)
    (* b 3)
    (f (g a))))