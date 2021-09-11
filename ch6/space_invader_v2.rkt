;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space_invader_v2) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 

;----

; Physical constants
(define SPEED-UFO 3)
(define SPEED-TANK 0)
(define SPEED-MISSILE (* SPEED-UFO 2))


; Graphical constants:
; - background
(define BG-WIDTH 200)
(define MID (/ BG-WIDTH 2))
(define BG-HEIGHT 200)

(define HEIGHT 180)
(define TANK-HEIGHT 10)

(define BACKGROUND (empty-scene BG-WIDTH BG-HEIGHT))

; - other stuffs
(define UFO (rectangle 20 10 "solid" "green"))
(define TANK (rectangle 20 TANK-HEIGHT "solid" "silver"))
(define MISSILE (triangle 10 "solid" "black"))


; Here is an instance that describes the tank maneuvering into position
; to fire the missile:
(define start (make-sigs (make-posn MID 20)
                        (make-tank MID -3)
                        #false))

; This one is just like the previous one but the missile has been fired:
(define fire (make-sigs (make-posn 20 10)
                          (make-tank 28 -3)
                          (make-posn 28 (- HEIGHT TANK-HEIGHT))))

; Finally, here is one where the missile is about to collide with the UFO:
(define collide (make-sigs (make-posn 20 100)
                            (make-tank 100 3)
                            (make-posn 22 103)))

; This one is just for test, make ufo landed:
(define land (make-sigs (make-posn 20 (- BG-HEIGHT (/ (image-height UFO) 2)))
                         (make-tank 100 3)
                         (make-posn 22 103)))
;----

;  ---------
; | TO-DRAW |
;  ---------

; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK
               (tank-loc t)
               HEIGHT
               im))
 
; UFO Image -> Image 
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO
               (posn-x u)
               (posn-y u)
               im))

; MISSILEorNot Image -> Image
; add m to the given image im
(define im (tank-render (make-tank MID -3)
                        (ufo-render (make-posn MID 20) BACKGROUND)))
(check-expect (missile-render.v2 #false im) im)
(check-expect (missile-render.v2 (make-posn 100 100) im)
              (tank-render (make-tank MID -3)
                           (ufo-render (make-posn MID 20)
                                       (place-image MISSILE
                                                    100
                                                    100
                                                    BACKGROUND))))
 
(define (missile-render.v2 m im)
  (cond
    [(boolean? m) im]
    [(posn? m)
     (place-image MISSILE
                  (posn-x m)
                  (posn-y m)
                  im)]))


; SIGS.v2 -> Image 
; renders the given game state on top of BACKGROUND 
(define (si-render.v2 s)
  (tank-render
    (sigs-tank s)
    (ufo-render (sigs-ufo s)
                (missile-render.v2 (sigs-missile s)
                                   BACKGROUND))))



;  -----------
; | STOP-WHEN |
;  -----------

; UFO -> Boolean
; check whether the ufo lands on the ground
(check-expect (ufo-lands? (make-posn 1
                                     (- BG-HEIGHT (/ (image-height UFO) 2))))
              #true)
(check-expect (ufo-lands? (make-posn 1
                                     (- BG-HEIGHT (image-height UFO))))
              #false)
                          
(define (ufo-lands? u)
  (>= (posn-y u) (- BG-HEIGHT (/ (image-height UFO) 2))))


; Posn Image -> Number
; get the four side's coordinate for a given position and image
(define testim (rectangle 10 10 "solid" "black"))
(define testp (make-posn 10 20))

(check-expect (image-left  testp testim) 5)
(check-expect (image-right testp testim) 15)
(check-expect (image-top testp testim) 15)
(check-expect (image-bottom testp testim) 25)

(define (image-left p im)
  (- (posn-x p) (/ (image-width im) 2)))
(define (image-right p im)
  (+ (posn-x p) (/ (image-width im) 2)))
(define (image-top p im)
  (- (posn-y p) (/ (image-height im) 2)))
(define (image-bottom p im)
  (+ (posn-y p) (/ (image-height im) 2)))


; UFO MISSILE -> Boolean
; check if missile hits the ufo
(check-expect (collide? (sigs-ufo fire) (sigs-missile fire))
              #false)
(check-expect (collide? (sigs-ufo collide) (sigs-missile collide))
              #true)

(define (collide? u m)
  (and (and (> (image-right m MISSILE)
               (image-left u UFO))
           (< (image-left m MISSILE)
              (image-right u UFO)))
       (and (< (image-top m MISSILE)
               (image-bottom u UFO))
           (> (image-bottom m MISSILE)
              (image-top u UFO)))))


; SIGS -> Boolean
; The game stops if the UFO lands or the missile hits the UFO
(check-expect (si-game-over start) #false)
(check-expect (si-game-over fire) #false)
(check-expect (si-game-over collide) #true)
(check-expect (si-game-over land) #true)

(define (si-game-over s)
  (cond
    [(equal? (sigs-missile s) #false) (ufo-lands? (sigs-ufo s))]
    [else (or (ufo-lands? (sigs-ufo s))
              (collide? (sigs-ufo s)
                        (sigs-missile s)))]))

(define WIN (overlay (text "YOU WIN" 20 "gold") BACKGROUND))
(define LOSE (overlay (text "YOU LOSE" 20 "red") BACKGROUND))

(define (si-render-final s)
  (cond
    [(equal? (sigs-missile s) #false) LOSE]
    [else (if (ufo-lands? (sigs-ufo s))
                    LOSE
                    WIN)]))

;  ---------
; | ON-TICK |
;  ---------

; -> Number
; get a random number from -n to n
(define (get-random n)
  (- (random (+ (* n 2) 1)) n))


; Posn -> Posn
; determin to which position the ufo moves to
(define testufo (make-posn 10 10))
(check-expect (ufo-descend testufo 5) (make-posn 15 (+ SPEED-UFO 10)))

(define (ufo-descend u shift)
  (make-posn (+ (posn-x u) shift)
             (+ (posn-y u) SPEED-UFO)))


; Posn -> Posn
; determin to which position the missile moves to
(define testmissile (make-posn 10 10))
(check-expect (missile-ascend testmissile) (make-posn 10 (- 10 SPEED-MISSILE)))

(define (missile-ascend m)
  (make-posn (posn-x m)
             (- (posn-y m) SPEED-MISSILE)))


; Tank -> Tank
; determin to which position the tank moves to
(define testtank (make-tank 10 3))
(check-expect (tank-move testtank) (make-tank 13 3))

(define (tank-move t)
  (make-tank (+ (tank-loc t) (tank-vel t))
             (tank-vel t)))


; SIGS -> SIGS
; determine to which position the objects move now
(check-random (si-move-proper start (get-random SPEED-UFO))
              (make-sigs (ufo-descend (sigs-ufo start) (get-random SPEED-UFO))
                         (tank-move (sigs-tank start))
                         #false))
(check-random (si-move-proper fire (get-random SPEED-UFO))
              (make-sigs (ufo-descend (sigs-ufo fire) (get-random SPEED-UFO))
                          (tank-move (sigs-tank fire))
                          (missile-ascend (sigs-missile fire))))

(define (si-move-proper s delta)
  (cond
    [(equal? (sigs-missile s) #false)
     (make-sigs (ufo-descend (sigs-ufo s) delta)
                (tank-move (sigs-tank s))
                #false)]
    [else
     (make-sigs (ufo-descend (sigs-ufo s) delta)
                (tank-move (sigs-tank s))
                (missile-ascend (sigs-missile s)))]))

(define (si-move s)
  (si-move-proper s (get-random SPEED-UFO)))


;  --------
; | ON-KEY |
;  --------

; SIGS -> SIGS
; if the status is aim now, fire the missile and change it to fired
(define (control-fire s)
  (cond
    [(equal? (sigs-missile s) #false)
     (make-sigs (sigs-ufo s)
                (sigs-tank s)
                (make-posn (tank-loc (sigs-tank s))
                           (- HEIGHT TANK-HEIGHT)))]
    [else s]))

; Tank -> Tank
; change tank's moving direction, i.e. change tank-vel field
(check-expect (change-direction (make-tank 10 3) "left")
              (make-tank 10 -3))
(check-expect (change-direction (make-tank 10 -3) "left")
              (make-tank 10 -3))
(check-expect (change-direction (make-tank 10 3) "right")
              (make-tank 10 3))
(check-expect (change-direction (make-tank 10 -3) "right")
              (make-tank 10 3))

(define (change-direction t d)
  (make-tank (tank-loc t)
             (cond
               [(key=? d "left") (- (abs (tank-vel t)))]
               [(key=? d "right") (abs (tank-vel t))])))


; SIGS -> SIGS
; change tank's moving direction, i.e. change aim-tank's or fired-tanks's
;  tank-vel field
(define (control-tank s direction)
  (cond
    [(equal? (sigs-missile s) #false)
     (make-sigs (sigs-ufo s)
                (change-direction (sigs-tank s) direction)
                #false)]
    [else
     (make-sigs (sigs-ufo s)
                (change-direction (sigs-tank s) direction)
                (sigs-missile s))]))

         
(define (si-control s ke)
  (cond
    [(key=? ke "left") (control-tank s ke)]
    [(key=? ke "right") (control-tank s ke)]
    [(key=? ke " ") (control-fire s)]))



;  ------
; | MAIN |
;  ------

(define (main s)
  (big-bang s
    [to-draw si-render.v2]
    [on-tick si-move 0.2]
    [on-key si-control]
    [stop-when si-game-over si-render-final]))
;(main start)