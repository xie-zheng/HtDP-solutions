;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32_4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

; ex508 - 509
; [List-of 1String] Number -> Editor
(define (split-structural los x)
  (local ((define (split-structural/a p s x)
            (cond
              [(empty? s) (make-editor p s)]
              [(<= (image-width (editor-text p))
                   x
                   (image-width (editor-text (append p (list (first s)))))) (make-editor p s)]
              [else (split-structural/a (append p (list (first s))) (rest s) x)])))
    (split-structural/a '() los x)))

; ex510
; Number String String
(define (fmt w in-f out-f)
  (local ((define f (read-words in-f))
          (define (fmt/a w in ac-line ac-txt)
            (cond
              [(empty? in) (string-append ac-txt ac-line)]
              [else
               (cond
                 [(> (+ (string-length (first in)) (string-length ac-line)) w)
                  (fmt/a w in "" (string-append ac-txt ac-line "\n"))]
                 [else
                  (fmt/a w
                         (rest in)
                         (string-append ac-line (first in) " ")
                         ac-txt)])])))
    (write-file out-f (fmt/a w f "" ""))))

