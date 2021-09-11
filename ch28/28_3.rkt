;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 28_3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))


; ex462
; SOE Solution -> Boolean
(define (check-solution e s)
  (andmap (lambda (r) (check-row r s)) e))
(check-satisfied M (lambda (m) (check-solution m S)))

; Equation Solution ->  Boolean
(define (check-row r s)
  (= (plug-in (lhs r) s) (rhs r)))
(check-expect (check-row (first M) S) #t)

; [List-of Number] Solution -> Number
(define (plug-in e s)
  (for/sum ([i e] [j s]) (* i j)))
(check-expect (plug-in (lhs (first M)) S) (rhs (first M)))


; Equation Equation -> Equation
(define (subtract e1 e2)
  (local ((define coef (/ (first e1) (first e2))))
    (rest (for/list ([i e1] [j e2]) (- i (* j coef))))))

; ex463
; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations 
(define (triangulate m)
  (cond
    [(empty? m) '()]
    [(all-zero? (col m 1)) (error "no pivot in first column")]
    [(= 0 (first (first m))) (triangulate (rotate-mat m))]
    [else (cons (first m)
                (triangulate (map (lambda (row) (subtract row (first m)))
                                  (rest m))))]))

(check-expect (triangulate M)
              '((2 2 3 10)
                (  3 9 21)
                (    1 2 )))
(check-expect
 (triangulate
  (list (list 2  3  3 8)
        (list 2  3 -2 3)
        (list 4 -2  2 4)))
 (list (list 2  3  3  8)
       (list   -8 -4 -12)
       (list      -5  -5)))

; SOE -> SOE
(define (rotate-mat m)
  (append (rest m) (list (first m))))

; SOE -> [List-of Number]
(define (col m i)
  (for/list ([r m]) (list-ref r i)))

; [List-of Number] -> Boolean
(define (all-zero? l)
  (andmap (lambda (i) (= i 0)) l))


; TM -> [List-of Number]
(define (solve e)
  (local ((define (solve-single eq sol)
            (cons (/ (- (rhs eq) (plug-in (rest (lhs eq)) sol)) (first eq))
                  sol)))
    (foldr (lambda (eq sol) (solve-single eq sol)) '() e)))

(define (gauss e)
  (solve (triangulate e)))
    