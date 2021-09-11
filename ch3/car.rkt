;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sample) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ========== CONSTANTS ==========

; car's properties
(define WHEEL-RADIUS 30)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 2))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS 0 "white"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR-WIDTH
  (* WHEEL-DISTANCE 4))
(define CAR-HEIGHT
  (* WHEEL-RADIUS 2))

(define TOP
  (rectangle (+ WHEEL-DISTANCE (* WHEEL-RADIUS 2))
                WHEEL-RADIUS
                "solid"
                "red"))
(define DOWN
  (rectangle CAR-WIDTH
             CAR-HEIGHT
             "solid"
             "red"))
(define BODY
  (above TOP DOWN))

(define CAR
  (underlay/align/offset "middle" "bottom"
                         BODY
                         0
                         WHEEL-RADIUS
                         BOTH-WHEELS))

(define tree
  (underlay/xy (circle WHEEL-RADIUS "solid" "green")
               (* WHEEL-RADIUS 0.9)
               (* WHEEL-RADIUS 1.5)
               (rectangle (* WHEEL-RADIUS 0.2)
                          (* WHEEL-RADIUS 2)
                          "solid" "brown")))

; world's constants
(define WIDTH-OF-WORLD
  (* WHEEL-RADIUS 20))
(define HEIGHT-OF-WORLD
  (* WHEEL-RADIUS 5))
(define MT
  (empty-scene WIDTH-OF-WORLD
               HEIGHT-OF-WORLD))

(define BACKGROUND
  (place-image/align tree
               (/ WIDTH-OF-WORLD 2)
               HEIGHT-OF-WORLD
               "left"
               "bottom"
               MT))

; ========== FUNCTIONS ==========

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car

; ----wish list----
; [done]render
; [done]clock-tick-handler
; []keystroke-handler
; []mouse-event-handler
; []end?
; -----------------

; WorldState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image 
(define (render x)
  (place-image/align CAR
                     x
                     HEIGHT-OF-WORLD
                     "left"
                     "bottom"
                     BACKGROUND))


; WorldState -> WorldState
; adds 3 to x to move the car right 
(define (tock x)
  (+ x 3))

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)


; WorldState -> Boolean
; indicate wether the car disappear on the right side
(define (end? ws)
  (if (> ws WIDTH-OF-WORLD) #true #false))

(check-expect (end? (+ WIDTH-OF-WORLD 1)) #true)
(check-expect (end? WIDTH-OF-WORLD) #false)


; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
; given: 21 10 20 "enter"
; wanted: 21
; given: 42 10 20 "button-down"
; wanted: 10
; given: 42 10 20 "move"
; wanted: 42
(define (hyper x-position-of-car x-mouse y-mouse me)
  (if (string=? me "button-down") x-mouse x-position-of-car))

(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)


; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [on-mouse hyper]
     [to-draw render]
     [stop-when end?]))



(main 0)
