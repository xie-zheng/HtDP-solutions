;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12_3) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; ex209
(define sample_s "cat")
(define sample_w (list "c" "a" "t"))

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '() or
; - (cons Word List-of-words)

; String -> Word
; converts s to the chosen word representation 
(define (string->word s)
  (explode s))
(check-expect (string->word sample_s) sample_w)
 
; Word -> String
; converts w to a string
(define (word->string w)
  (implode w))
(check-expect (word->string sample_w) sample_s)

; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))

; List-of-strings -> Boolean
(define (all-ar-from-rat? w)
  (and
    (member? (explode "rat") w) (member? (explode "rta") w)
    (member? (explode "art") w) (member? (explode "atr") w)
    (member? (explode "tar") w) (member? (explode "tra") w)))

; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))


; Word -> List-of-words
; creates all rearrangements of the letters in w
(check-expect (arrangements (list "a")) (list (list "a")))
(check-satisfied (arrangements (explode "rat")) all-ar-from-rat?)

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

; 1String List-of-words -> List-of-words
(define (insert-everywhere/in-all-words c words)
  (cond
    [(empty? words) '()]
    [else (append (insert-everywhere c (first words))
                  (insert-everywhere/in-all-words c (rest words)))]))

; 1String Word -> List-of-words
(check-expect (insert-everywhere "r" (explode "at"))
              (list (explode "rat") (explode "art") (explode "atr")))

(define (insert-everywhere c word)
  (combine '() (list c) word))

; Word Word Word -> Word
(define (combine pre c after)
  (cond
    [(empty? after) (cons (append pre c) '())]
    [else (cons (append pre c after)
                (combine (append pre (list (first after))) c (rest after)))]))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(define DICT-LOC "E:\\words")
(define DICT (read-lines DICT-LOC))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) DICT)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))