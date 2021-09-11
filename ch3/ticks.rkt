;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ticks) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ========== CONSTANTS ==========

; car's properties
(define WHEEL-RADIUS 20)
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
  (* WHEEL-RADIUS 50))
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

; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started

; ----wish list----
; [done]render
; [done]clock-tick-handler
; []keystroke-handler
; []mouse-event-handler
; []end?
; -----------------

; AnimationState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image 
(define (render as)
  (place-image/align CAR
                     (x-pos as)
                     (y-pos as)
                     "left"
                     "bottom"
                     BACKGROUND))


; AnimationState -> AnimationState
; adds 3 to x to move the car right 
(define (tock as)
  (+ as 1))

(check-expect (tock 20) 21)
(check-expect (tock 78) 79)

; AnimationState -> WorldState
; get the current x-position of the car 
(define (x-pos as)
  (* 3 as))


; AnimationState -> WorldState
; get the current x-position of the car
(define (y-pos as)
  (- HEIGHT-OF-WORLD (sin as)))


; AnimationState -> Boolean
; indicate wether the car disappear on the right side
(define (end? as)
  (if (> (x-pos as) WIDTH-OF-WORLD) #true #false))

; (check-expect (end? (+ WIDTH-OF-WORLD 1)) #true)
; (check-expect (end? WIDTH-OF-WORLD) #false)


; AnimationState -> AnimationState
; launches the program from some initial state 
(define (main as)
   (big-bang as
     [on-tick tock]
     [to-draw render]
     [stop-when end?]))

(main 0)
