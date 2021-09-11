;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |47|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define MAX 100)
(define decrease (/ 100 5))
(define increase (/ 100 3))

(define WIDTH 100)
(define HEIGHT 500)
(define ONE (/ HEIGHT MAX))

(define BG
  (empty-scene WIDTH HEIGHT))
; WorldState -> WorldState
(define (tock ws)
  (max (- ws 0.1) 0))

; WorldState -> WorldState
(define (ke-handler ws ke)
  (cond
    [(key=? ke "up") (min (+ ws increase) MAX)]
    [(key=? ke "down") (max (- ws decrease) 0)]
    [else (max ws 0)]))


; WorldState -> Image
(define (render ws)
  (place-image/align (text (number->string (round ws)) 20 "black")
                     (/ WIDTH 2)
                     20
                     "center"
                     "top"
                     (place-image/align (rectangle WIDTH (* ws ONE) "solid" "red")
                                        0
                                        HEIGHT
                                        "left"
                                        "bottom"
                                        BG)))

(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-key ke-handler]
    [to-draw render]))

;(main MAX)