;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 27_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; File -> [List-of Line]
; converts a file into a list of lines 
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
 
; File -> Line
; retrieves the prefix of afile up to the first occurrence of NEWLINE
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))
 
; File -> File
; drops the suffix of afile behind the first occurrence of NEWLINE
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
 
(define NEWLINE "\n") ; the 1String

(define LOWER (explode "abcdefghijklmnopqrstuvwxyz"))

; Line -> [List-of Tokens]
(define (tokenize line)
  (cond
    [(empty? line) '()]
    [else (if (member? (first line) LOWER)
              (cons (first line) (tokenize (rest line)))
              (tokenize (rest line)))]))

; [List-of Number] -> [List-of [List-of Number]]
(define (create-matrix n nums)
  (cond
    [(empty? nums) '()]
    [else (cons (first-row n nums) (create-matrix n (remove-row n nums)))]))

(define (first-row n nums)
  (cond
    [(or (= n 0) (empty? nums)) '()]
    [else (cons (first nums) (first-row (sub1 n) (rest nums)))]))

(define (remove-row n nums)
  (cond
    [(or (= n 0) (empty? nums)) nums]
    [else (remove-row (sub1 n) (rest nums))]))

(check-expect
  (create-matrix 2 (list 1 2 3 4))
  (list (list 1 2)
        (list 3 4)))