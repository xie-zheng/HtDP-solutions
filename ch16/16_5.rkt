;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 16_5) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; [List-of Posn] -> [List-of Posn]
; adds 3 to each x-coordinate on the given list 
 
(check-expect
 (add-3-to-all
   (list (make-posn 3 1) (make-posn 0 0)))
 (list (make-posn 6 1) (make-posn 3 0)))
 
(define (add-3-to-all lop)
  (local (;
          (define (add-3-to-x p)
            (make-posn (+ (posn-x p) 3)
                       (posn-y p))))
    (map add-3-to-x lop)))

(define (keep-good lop)
  (local (; Posn -> Posn
          ; should this Posn stay on the list
          (define (good? p)
            (<= (posn-y p) 100)))
    (filter good? lop)))

; Posn Posn Number -> Boolean
; is the distance between p and q less than d
(define (close-to p q d)
  (<= (distance-to p q) d))

; Posn Posn -> Number
(define (distance-to p q)
  (sqrt (+ (sqr (- (posn-x p) (posn-x q)))
           (sqr (- (posn-y p) (posn-y q))))))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt
 
(check-expect
 (close? (list (make-posn 47 54) (make-posn 0 60))
         (make-posn 50 50))
 #true)
 
(define (close? lop pt)
  (local (;
          (define (helper p)
            (close-to p pt 5)))
    ; - IN -
    (ormap helper lop)))