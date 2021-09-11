;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex109) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ExpectsToSee.v2 is one of:
; – AA
; – BB
; – DD 
; – ER 
 
(define AA "start, ...")
(define BB "expect ...")
(define DD "finished")
(define ER "error, ...")

(define WIDTH 100)
(define HEIGHT 100)

(define INIT (rectangle WIDTH HEIGHT "solid" "white"))
(define FIRST (rectangle WIDTH HEIGHT "solid" "yellow"))
(define END (rectangle WIDTH HEIGHT "solid" "green"))
(define ERROR (rectangle WIDTH HEIGHT "solid" "red"))


; ExpectsToSee.v2 -> ExpectsToSee.v2
(define (transit current ke)
  (cond
    [(equal? current AA) (if (key=? ke "a") BB ER)]
    [(equal? current BB) (if (or (key=? ke "b") (key=? ke "c"))
                             BB
                             (if (key=? ke "d") DD ER))]
    [(equal? current DD) DD]
    [(equal? current ER) ER]))


; ExpectsToSee.v2 -> Image
(define (render current)
  (cond
    [(equal? current AA) INIT]
    [(equal? current BB) FIRST]
    [(equal? current DD) END]
    [(equal? current ER) ERROR]))


(define (main l)
  (big-bang l
    [on-key transit]
    [to-draw render]))
