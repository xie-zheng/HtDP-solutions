;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 4_3) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; 50

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")

(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))


; 51
(define RADIUS 100)
(define WIDTH (* RADIUS 2.4))
(define HEIGHT (* RADIUS 2.4))
(define BG (rectangle  WIDTH HEIGHT 0 "white"))

; TrafficLight, Boolean -> Image
; get the light image for a given state
; if on, get a transparent light
; else, get a solid light
(define (light color on/off)
  (overlay
   (circle RADIUS (if on/off 255 50) color)
   BG))


; TrafficLight -> Image
; Get the current traffic light depends on
; which color is on
(define (lights color)
  (above
   (light "red" (string=? "red" color))
   (light "yellow" (string=? "yellow" color))
   (light "green" (string=? "green" color))))


; TrafficLight rate-expr -> World
; rate-expr : (and/c real? positive?)
(define (main s duration)
  (big-bang s
    [to-draw lights]
    [on-tick traffic-light-next duration]))

(main "red" 2)