;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5_9) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct ufo [y speed])
; An ufo is a structure:
;    (make-ufo [Number Number])
; interpretaion a ufo decending at constant speed
; its current y-coordinate is y


(define-struct tank [x speed])
; A tank is a structure:
;   (make-tank [Number Number])
; interpretaion a tank moving at constant speed
; its current x-coordinate is x

(define-struct space-game [ufo tank])
; A space-game is a structure:
;   (make-space-game ufo tank)



