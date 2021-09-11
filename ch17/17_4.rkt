;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 17_4) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; - ex293 -
; Develop found?, a specification for the find function
; [X] [List-of X] -> [[Maybe [List-of X]] -> Boolean]
(define (found? x l)
  (lambda (mb)
    (cond
      [(boolean? mb) (and (not mb) (not (member? l)))]
      [else (and (list? l)
                 (equal? (first mb) x)
                 (memq? mb l)
                 (local (; helper
                         ; [X] X Number -> Number
                         (define (if-eq-add1 item count)
                           (+ count (if (equal? item x) 1 0)))
                         ; [X] [List-of X] -> Number
                         (define (counter lx)
                           (foldr if-eq-add1 0 lx)))
                   ; - IN -
                   (equal? (counter mb) (counter l))))])))



; Exercise 294.
; Develop is-index?, a specification for index
; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))
(check-satisfied (index 5 '(9 4 5 1 2))
                 (is-index? 5 '(9 4 5 1 2)))

; [X] X [List-of X] -> [[Maybe N] -> Boolean
(define (is-index? x l)
  (lambda (n)
    (cond
      [(boolean? n) (and (not n) (not (member? x l)))]
      [else (at-nth? l n x)])))

; [X] [List-of X] X Number -> Boolean
(define (at-nth? l n x)
  (cond
    [(empty? l) #f]
    [(= n 0) (equal? (first l) x)]
    [else (if (equal? (first l) x)
              #f
              (at-nth? (rest l) (- n 1) x))]))

; Exercise 295.
; Develop n-inside-playground?, a specification of the random-posns function
; below. The function generates a predicate that ensures that the length of the
; given list is some given count and that all Posns in this list are within a
; WIDTH by HEIGHT rectangle
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))


; N -> [[List-of Posn] -> Boolean]
(define (n-inside-playground? n)
  (lambda (l0)
    (and (= n (length l0))
         (andmap (lambda (p) (and (< (posn-x p) WIDTH)
                                  (< (posn-y p) HEIGHT)))
                 l0))))

; ...
(define (random-posns/bad n)
  (build-list
   n
   (lambda (i)
     (make-posn 0 0))))
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))

  


           