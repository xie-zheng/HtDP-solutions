;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 28_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require plot)

(define ε 0.01)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(define (slope f r1)
  (/ (- (f (+ r1 ε)) (f (- r1 ε))) (* 2 ε)))

(define (root-of-tangent f r1)
  (- r1 (/ (f r1) (slope f r1))))

; [Number -> Number] Number -> Number
; finds a number r such that (f r) is small
; generative repeatedly generates improved guesses
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) ε) r1]
    [else (newton f (root-of-tangent f r1))]))

(check-within (newton poly 1) 2 ε)
(check-within (newton poly 3.5) 4 ε)

; Number -> Number
; ...
(define (double-amount r)
  (local ((define (helper current r n)
            (cond
              [(>= current 2) n]
              [else (helper (* (+ 1 r) current) r (add1 n))])))
    (helper 1 r 0)))
