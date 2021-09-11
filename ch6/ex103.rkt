;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex103) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct spider [space leg])
; A Spider is a struct：
;   (make-spider Number Number)
; interpretation (make-spider 10 8) represents a spider with 8 legs
; and needs 10 of space in transport

(define-struct elephant [space])
; An Elephant is a struct:
;    (make-elephant Number)
; interpretation (make-elephant 20) represents a elephant that
; needs 20 of space in transport

(define-struct bos-constrictor [length girth])
; A Bos-constrictor is a struct:
;   (make-bos-constrictor Number Number)
; interpretation (make-bos-constrictor a b) represents a bos-constrictor
; whose length is a and girth is b

(define-struct armadillo [space age])
; An Armadillo is a struct:
;    (make-armadillo Number Number)
; interpretation (make-armadillo a b) represents a armadillo needs
; a of space and b years old

(define-struct cage [x y z])
; A cage is a struct:
;   (make-cage Number Number Number)
; interpretation (make-cage a b c) represents a cage whose lengths are
; a, b, c

(define (zoo-template animal)
  (cond
    [(spider? animal) ...]
    [(elephant? animal) ...]
    [(bos-constrictor? animal) ...]
    [(armadillo? animal) ...]))

; Bos-constrictor -> Number
(check-expect (bos-constrictor-space (make-bos-constrictor 10 10))
              100)
(define (bos-constrictor-space b)
  (* (bos-constrictor-length b)
     (bos-constrictor-girth b)))

; Cage -> Number
; get the volume of a given cage
(check-expect (volume (make-cage 10 10 10)) (* 10 10 10))
(check-expect (volume (make-cage 10 10 0)) (* 10 10 0))
(define (volume c)
  (* (cage-x c) (cage-y c) (cage-z c)))

; Number Cage -> Boolean
; whether the cage's volume is greater than vol
(define test-cage (make-cage 3 4 5))
(define test-cage-vol (* 3 4 5))
(check-expect (fitsin? 0 test-cage) #true)
(check-expect (fitsin? (+ 10 test-cage-vol) test-cage) #false)
(define (fitsin? vol c)
  (if (>= (volume c) vol) #true #false))


; Animal Cage -> Boolean
(check-expect (fits? (make-spider 10 10) test-cage) #true)
(check-expect (fits? (make-elephant 100) test-cage) #false)
(check-expect (fits? (make-bos-constrictor 10 10) test-cage) #false)
(check-expect (fits? (make-armadillo 10 10) test-cage) #true)
(define (fits? animal cage)
  (fitsin?
   (cond
     [(spider? animal) (spider-space animal)]
     [(elephant? animal) (elephant-space animal)]
     [(bos-constrictor? animal) (bos-constrictor-space animal)]
     [(armadillo? animal) (armadillo-space animal)])
   cage))
