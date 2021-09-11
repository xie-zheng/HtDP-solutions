;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 8_3) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define (flatt? s)
  (string=? s "Flatt"))

; List-of-names -> Boolean
; 检查名字"Flatt"是否在列表之中
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Mike" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect
 (contains-flatt? (cons "Mike"
                        (cons "Flatt" '())))
 #true)
(check-expect
 (contains-flatt? (cons "Mike"
                        (cons "Howard" '())))
 #false)
(check-expect (contains-flatt? "abc") #false)

(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon) (if (flatt? (first alon))
                      #true
                      (contains-flatt? (rest alon)))]
    [else #false]))


; ---

; ex134
(define (contains? name alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon) (if (string=? (first alon) name)
                      #true
                      (contains-flatt? (rest alon)))]
    [else #false]))

; ex135
(contains-flatt? (cons "Flatt" (cons "C" '())))