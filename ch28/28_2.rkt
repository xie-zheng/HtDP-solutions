;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 28_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))


; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
(define (integrate f l r)
  (integrate-kepler f l r))

(check-expect (integrate constant 12 22) 200)
(check-expect (integrate linear 0 10) 100)
(check-expect (integrate square 0 10)
              (- (expt 10 3) (expt 0 3)))

 
; [Number -> Number] Number Number -> Number

; assume (< a b) holds 
 
(check-within (integrate (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)

; trap is a struct
;   (make-trap Posn Posn)
(define-struct trap [ul ur])

(define (integrate-kepler f l r)
  (local ((define mid (/ (+ l r) 2)))
    (+ (trap-area (make-trap (make-posn l (f l))
                             (make-posn mid (f mid))))
       (trap-area (make-trap (make-posn mid (f mid))
                             (make-posn r (f r)))))))

(define (trap-area trap)
  (local ((define l (posn-x (trap-ul trap)))
          (define r (posn-x (trap-ur trap)))
          (define fl (posn-y (trap-ul trap)))
          (define fr (posn-y (trap-ur trap)))
          (define width (- r l)))
    (cond
      [(> fl fr) (+ (* fr width) (/ (* (- fl fr) width) 2))]
      [(<= fl fr) (+ (* fr width) (/ (* (- fl fr) width) 2))])))


(define (integrate-rectangles f l r n)
  (local ((define w (/ (- r l) n))
          (define s (/ w 2))
          (define (add-blocks l)
            (cond
              [(= l r) 0]
              [else (+ (* w (f (+ l s)))
                       (add-blocks (+ l w)))])))
    (add-blocks l)))

(define (integrate-dc f l r)
  (local ((define mid (/ (+ l r) 2))
          (define area (integrate-kepler f l r))
          (define area-l (integrate-kepler f l mid))
          (define area-r (integrate-kepler f mid r))
          (define precision (* ε (- r l)))
          (define diff (abs (- area (+ area-l area-r)))))
    (cond
      [(< diff precision) area]
      [else (+ (integrate-dc f l mid)
               (integrate-dc f mid r))])))

(check-expect (integrate-dc constant 12 22) 200)
(check-expect (integrate-dc linear 0 10) 100)
(check-expect (integrate-dc square 0 10)
              (- (expt 10 3) (expt 0 3)))