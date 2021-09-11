;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 22_2) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;=======================================================================
(define a0 '((initial "X")))
 

(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))
;=======================================================================
; Any -> Boolean
(define (attribute? x)
  (and (list? x)
       (= (length x) 2)
       (symbol? (first x))
       (string? (second x))))

; Any -> Boolean
(define (attributes? lox)
  (and (list? lox)
       (andmap attribute? lox)))
;=======================================================================
; Xexpr.v2 -> [List-of Xexpr.v2]
(define (xexpr-content xe)
  (local ((define content (rest xe)))
    (match content
      [(cons (? attributes?) body) (rest content)]
      [body body])))

(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
;=======================================================================

; An XWord is '(word ((text String)))
; ex370
(define w0 '(word ((text "hello"))))
; Xexpr -> Boolean
(define (word? ex)
  (match ex
    [(cons 'word
           (cons
            (cons
             (cons 'text (cons str '())) '()) '()))
     #t]
    [expr #f]))

; XWord -> String
(define (word-text ex)
  (match ex
    [(cons 'word
           (cons
            (cons
             (cons 'text (cons str '())) '()) '()))
     str]))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define BT1 (circle 3 "solid" "black"))
(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT1 (text "one" 12 'black))
   (beside/align 'center BT1 (text "two" 12 'black))))

; XItem.v1 -> Image 
; renders an item as a "word" prefixed by a bullet 
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT1 item)))

(check-expect (render-item1 '(li (word ((text "one")))))
              (beside/align 'center BT1 (text "one" 12 'black)))

; XEnum.v1 -> Image 
; renders a simple enumeration as an image 
(define (render-enum1 xe)
 (local ((define content (xexpr-content xe))
         (define texts (map render-item1 content))
         (define (combine up down) (above/align 'left up down)))
   (foldr combine empty-image texts)))

(check-expect (render-enum1 e0) e0-rendered)
;=======================================================================

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons Attributes (cons XEnum.v2 '())))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BT ; a graphical constant 
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))
 
; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BT item))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image 
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))
(check-expect (render-enum e0)
              (above/align
               'left
               (render-item '(li (word ((text "one")))))
               (render-item '(li (word ((text "two")))))))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE 'black)]
        [else (render-enum content)]))))
(check-expect (render-item '(li (word ((text "one")))))
              (bulletize (text "one" SIZE COLOR)))


(define (count-enum ex word)
  (local ((define content (xexpr-content ex)))
    (for/sum ([item content]) (count-item item word))))

(define (count-item item word)
  (local ((define content (first (xexpr-content item))))
    (cond
      [(word? content) (if (string=? (word-text content) word) 1 0)]
      [else (count-enum content word)])))
