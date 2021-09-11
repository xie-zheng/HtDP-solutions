;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 18_1) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define-struct child [father mother name date eyes])
; A Child is a structure: 
;   (make-child Child Child String N String)

(define-struct no-parent [])
(define NP (make-no-parent))

; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))





(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #f]
    [else (or (equal? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))


; ex310
; FT -> Number
(define (count-persons an-ftree)
  (length (list-persons an-ftree)))

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)


(define (list-persons an-ftree)
  (cond
    [(no-parent? an-ftree) '()]
    [else (set+ an-ftree
                (set+ (list-persons (child-father an-ftree))
                      (list-persons (child-mother an-ftree))))]))

(define (set+ set1 set2)
  (cond
    [(child? set1) (if (member? set1 set2) set2 (cons set1 set2))]
    [else (toset (append set1 set2))]))

(define (toset s)
  (foldr set+ '() s))





; ------
 An FF (short for family forest) is
; [List-of Family]
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees
 
; FF -> Boolean
; does the forest contain any child with "blue" eyes
 (define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
 
(define (blue-eyed-child-in-forest? a-forest)
  (ormap blue-eyed-child? a-forest))