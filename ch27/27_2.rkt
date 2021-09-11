;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 27_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require plot)
(define ε 0.001)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption 
(define (find-root f left right)
  (find-root-ac f left right (f left) (f right)))

(define (find-root-ac f left right f@l f@r)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= f@l 0 f@mid) (<= f@mid 0 f@l))
           (find-root-ac f left mid f@l f@mid)]
          [(or (<= f@mid 0 f@r) (<= f@r 0 f@mid))
           (find-root-ac f mid right f@mid f@r)]))]))

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-satisfied find-root (lambda (f) (= 0 (poly (f poly 3 8)))))


; ex450
(define (find-root-mono f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(> f@mid 0) (find-root-mono f left mid)]
          [(< f@mid 0) (find-root-mono f mid right)]
          [else mid]))]))
(plot3d (surface3d (lambda (x y) (find-root poly x y)) 0 3 3 8))