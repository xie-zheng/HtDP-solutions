;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32_2) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define (invert alox)
  (local ((define (invert/a alox accu)
            (cond
              [(empty? alox) accu]
              [else (invert/a (rest alox)
                              (cons (first alox) accu))])))
    (invert/a alox '())))

(check-expect (invert '(a b c)) '(c b a))