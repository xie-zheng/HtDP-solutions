;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5_8) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;----------------------------------------
(define-struct r3 [x y z])
; An R3 is a structure:
;   (make-r3 Number Number Number)
 
(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))


; R3 -> Number
; calculate R3's distance to the origin
(check-within (distance ex1)
              (sqrt (+ (sqr 1)
                       (sqr 2)
                       (sqr 13)))
              0.00000000000001)
(check-within (distance ex2)
              (sqrt (+ (sqr -1)
                       (sqr 0)
                       (sqr 3)))
              0.00000000000001)

(define (distance point)
  (sqrt (+ (sqr (r3-x point))
           (sqr (r3-y point))
           (sqr (r3-z point)))))
;----------------------------------------


; ex81
(define-struct time [hours minutes seconds])
; A time is a sturcture:
;   (make-time Hour Minute Second)
; Hour is a positive number in [0, 24]
; Minute is a positive number in [0, 60]
; Second is a positive number in [0, 60]
; time represents a point in time since midnight
(define time1 (make-time 11 20 35))
(define time2 (make-time 11 53 40))

; time -> Number
; represent the current time in seconds
;(check-expect (time->seconds time1) (+ 35
;                                       (* 60 (+ 20
;                                               (* 60 11)))))
;(check-expect (time->seconds time2) (+ 40
;                                       (* 60 (+ 53
;                                               (* 60 11)))))
;(define (time->seconds t)
;  (... (time-hours t) ... (time-minutes t) ... (time-seconds t) ... ))



; ex82
(define-struct three-letter-word [l1 l2 l3]) 
; A three-letter-word is a structure:
;   (make-three-letter-word LB LB LB)
; LB is one of the:
; - 1String
; - #false

; LB LB -> LB
; compare a single letter
(check-expect (compare-letter "a" #false) #false)
(check-expect (compare-letter "a" "b") #false)
(check-expect (compare-letter #false #false) #false)
(check-expect (compare-letter "a" "a") "a")
(define (compare-letter l1 l2)
  (if (equal? l1 l2) l1 #false))

; three-letter-word three-letter-word -> three-letter-word
; consumes two two three-letter words. It produces
; a word that indicates where the given ones agree and disagree
(check-expect (compare-word
               (make-three-letter-word "a" "b" #false)
               (make-three-letter-word "a" "c" "d"))
              (make-three-letter-word "a" #false #false))
(define (compare-word word1 word2)
  (make-three-letter-word (compare-letter (three-letter-word-l1 word1)
                                          (three-letter-word-l1 word2))
                          (compare-letter (three-letter-word-l2 word1)
                                          (three-letter-word-l2 word2))
                          (compare-letter (three-letter-word-l3 word1)
                                          (three-letter-word-l3 word2))))