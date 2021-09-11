;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 16_7) (read-case-sensitive #t) (teachpacks ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "web-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ex267
; [List-of Number] Number -> [List-of Number]
; converts a list of USD into a list of EURO
(define EXCHANGE-RATE 1.06)
(define (convert-euro lon rate)
  (local (; change usd to euro
          (define (usd-to-euro amount)
            (/ amount rate)))
    ; - IN -
    (map usd-to-euro lon)))

; ex268
; An inventory record specifies the name of
; an item,
; a description,
; the acquisition price,
; and the recommended sales price.
(define-struct inv [name description acquire sale])
; [List-of Inventory] -> [List-of Inventory]
; sort list-of Inv by acquisition price and sales prices
(define (sort-by-profits loi)
  (sort loi profits>))

; Inventory Inventory -> Boolean
(define (profits> i1 i2)
 (> (profits i1)
    (profits i2)))

; Inventory -> Number
(define (profits i)
  (- (inv-sale i)
     (inv-acquire i)))



; ex269
; Number [List-of Inventory] -> [List-of Inventory]
(define (eliminate-expensive ua loi)
  (local ((define (belowUA? i)
            (<= (inv-sale i) ua)))
    ; - IN -
    (filter belowUA? loi)))



; ex270
(define (one n)
  (local ((define (f x) x))
    (build-list n f)))

(define (two n)
  (build-list n add1))

(define (three n)
  (local ((define (f x) (/ 1 x)))
    (build-list n f)))



; ex271
(define (find-name name lon)
  (local ((define (find? n)
            (string-contains? name n)))
    ; - IN -
    (ormap find? lon)))



; ex272
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

;(append-from-fold '(1 2 3 4) '(5 6 7 8))



; ex273
(define (map-from-fold f l)
  (local ((define (map-and-cons item1 item2)
            (cons (f item1) item2)))
    (foldr map-and-cons '() l)))

(define (remove-after l n)
  (cond
    [(= n 0) '()]
    [else (cons (first l)
                (remove-after (rest l) (- n 1)))]))

(define (prefixes l)
  (local ((define (nth n)
            (remove-after l n)))
    (rest (build-list (length l) nth))))

(define (suffixes l)
  (local ((define (rest-and-cons l)
            (cond
              [(empty? l) '()]
              [else (cons l
                     (rest-and-cons (rest l)))])))
    (rest-and-cons (rest l))))
  