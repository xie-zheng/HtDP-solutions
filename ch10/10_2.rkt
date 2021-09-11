;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 10_2) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low -> List-of-numbers
; computes the weekly wages for the given records
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons (for-work (first an-low))
           (wage*.v2 (rest an-low)))]))

(check-expect
 (wage*.v2
  (cons (make-work "Robby" 11.95 39) '()))
 (cons (* 11.95 39) '()))

(check-expect
 (wage*.v2
  (cons (make-work "Matthew" 12.95 45)
        (cons (make-work "Robby" 11.95 39)
              '())))
 (cons (* 12.95 45)
       (cons (* 11.95 39)
             '())))


; Work -> ???
; a template for processing elements of Work
(define (for-work w)
  (* (work-rate w) (work-hours w)))




(define-struct paycheck [employee amount])
; A paycheck is a structure:
;   (make-paycheck String Number)
; interpretation (make-paycheck n a) combines the name 
; with the amount of money a
(define (wage*.v3 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons (checking (first an-low))
           (wage*.v3 (rest an-low)))]))

; Work -> Paycheck
(define (checking w)
  (make-paycheck (work-employee w)
                 (for-work w)))


(define-struct work.v2 [id name rate hours])
(define-struct paycheck.v2 [id name amount])


(define (wage*.v4 an-low)
  (cond
   [(empty? an-low) '()]
   [(cons? an-low)
    (cons (checking.v2 (first an-low))
          (wage*.v4 (rest an-low)))]))

; Work.v2 -> Paycheck.v2
(define (checking.v2 w)
  (make-paycheck.v2 (work.v2-id w)
                    (work.v2-name w)
                    (* (work.v2-rate w) (work.v2-hours w))))

; ex 167-170
; too easy