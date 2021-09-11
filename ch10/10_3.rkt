;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 10_3) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; An LN is one of: 
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))
 
; LN -> List-of-numbers
; determines the number of words on each line 
 
(check-expect (words-on-line ln0) '())
(check-expect (words-on-line ln1) (cons 2 (cons 0 '())))
 
(define (words-on-line ln)
  (cond
    [(empty? ln) '()]
    [else (cons (length (first ln))
                (words-on-line (rest ln)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))



; ex172
; List-of-lines -> String
(define (collapse lol)
  (cond
    [(empty? lol) ""]
    [else (string-append (combine (first lol))
                         (if (empty? (rest lol)) "" "\n")
                         (collapse (rest lol)))]))

; List-of-words -> String
(define (combine low)
  (cond
    [(empty? low) ""]
    [else (string-append (first low)
                         (if (empty? (rest low)) "" " ")
                         (combine (rest low)))]))


; ex174
; Remove all articles from a text file
; An article is one of "a", "an", "the"
(define (article? w)
  (or (string=? w "a")
      (string=? w "an")
      (string=? w "the")))

; List-of-words ->
(define (line-remove-article l)
  (cond
   [(empty? l) '()]
   [else (cond
           [(article? (first l)) (line-remove-article (rest l))]
           [else (cons (first l) (line-remove-article (rest l)))])]))

; List-of-lines ->
(define (lines-remove-article lol)
  (cond
   [(empty? lol) '()]
   [else (cons (line-remove-article (first lol))
               (lines-remove-article (rest lol)))]))

; String ->
(define (remove-article n)
  (write-file (string-append "no-articles-" n)
              (collapse (lines-remove-article (read-words/line n)))))

; ex175
; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))
; Encodes text files numerically

; String -> List-of-list-of-String
(define (encode-text n)
  (encode-all (read-words/line n)))

(define (encode-all lol)
  (cond
    [(empty? lol) '()]
    [else (cons (encode-line (first lol))
                (encode-all (rest lol)))]))

; List-of-String -> List-of-list-of-String
(define (encode-line low)
  (cond
    [(empty? low) '()]
    [else (cons (encode-word (first low))
                (encode-line (rest low)))]))

; String -> List-of-String
(define (encode-word w)
  (encode-letters (explode w)))
(check-expect (encode-word "ttt")
              (list "116" "116" "116"))

; List-of-1String -> List-of-String
(define (encode-letters lol)
  (cond
    [(empty? lol) '()]
    [else (cons (encode-letter (first lol))
                (encode-letters (rest lol)))]))
(check-expect (encode-letters (explode "ttt"))
              (list "116" "116" "116"))

; ??????????????????????????????????????????????????
; String -> Number, Number, Number
(define (wc n)
  (list (string-length (read-file n))
        (count-words (read-words/line n))
        (count-lines (read-lines n))))
;  ???????????????????????????????????????????????????????




(define (count-chars lol)
  (cond
    [(empty? lol) 0]
    [else (+ (string-length (first lol))
             (count-chars (rest lol)))]))

(define (count-words lol)
  (cond
    [(empty? lol) 0]
    [else (+ (length (first lol))
             (count-words (rest lol)))]))

(define (count-lines lol)
  (length lol))


; ex175
; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
(check-expect (transpose mat1) tam1)
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; Matrix -> List-of-Number
(define (first* lln)
  (cond
    [(empty? lln) '()]
    [else (cons (first (first lln))
                (first* (rest lln)))]))

(define (rest* lln)
  (cond
    [(empty? lln) '()]
    [else (cons (rest (first lln))
                (rest* (rest lln)))]))