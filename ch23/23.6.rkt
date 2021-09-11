;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |23.6|) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;[X] [set-of X] [set-of X] -> [set-of X]
(define (union set1 set2)
  (foldr (lambda (x y) (if (member? x set2) y (cons x y)))
         set2
         set1))

(define (intersect set1 set2)
  (foldr (lambda (x y) (if (member? x set2) (cons x y) y))
         '()
         set1))


; [List-of Number] [List-of Number] -> [List-of Number]
(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(<= (first l1) (first l2))
     (cons (first l1)
           (merge (rest l1) l2))]
    [(> (first l1) (first l2))
     (cons (first l2)
           (merge l1 (rest l2)))]))


; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke the-word)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word Key-Event -> HM-Word
(define (compare-word word status ke)
  (cond
    [(empty? word) '()]
    [else (local ((define check (and
                                 (string=? (first word) ke)
                                 (string=? (first status) "_")))
                  (define char (if check ke (first status))))
            (cons char (compare-word (rest word) (rest status) ke)))]))

(define LOCATION "E:\\words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(play (list-ref AS-LIST (random SIZE)) 10)


; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
; see exercise 213
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else (for*/list ([item w]
                      [arrangement-without-item
                       (arrangements (remove item w))])
            (cons item arrangement-without-item))]))

(define (random-pick l)
  (list-ref l (random (length l))))

; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [else (if (any-same names (first ll))
              (non-same names (rest ll))
              (cons (first ll) (non-same names (rest ll))))]))

(define (anysame names l)
  (ormap (for/list ([name names] [other l]) (string=? name other))))
           