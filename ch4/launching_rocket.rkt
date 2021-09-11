;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname launching_rocket) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ------------------------
;|Racket's error handling?|
; ------------------------

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)   ; speed of the rocket

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))


; LRCD -> Image
; place rocket at x
(define (rocket x)
  (place-image ROCKET 10 x BACKG))

; LRCD -> Image
; place countdown number on a resting rocket
(define (countdown x)
  (place-image (text (number->string x) 20 "red")
               10 (* 3/4 WIDTH)
               (rocket HEIGHT)))

; LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect
 (show "resting")
 (place-image ROCKET 10 HEIGHT BACKG))
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 HEIGHT BACKG)))
(check-expect
 (show 53)
 (place-image ROCKET 10 (- HEIGHT 53) BACKG))

(check-expect (show "abc") "wrong")

(define (show x)
  (cond
    [(and (string? x) (string=? x "resting")) (rocket HEIGHT)]
    [(number? x) (cond
                   [(<= -3 x -1) (countdown x)]
                   [(<= 0 x) (rocket (- HEIGHT x))]
                   [else "wrong"])]
    [else "wrong"]))


; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
  (if (and (string? x) (string=? "resting"))
      (if (key=? ke " ")
          -3
          x)
      x))
 
; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already 
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(number? x) (cond
                   [(<= -3 x -1) (+ x 1)]
                   [(<= 0 x) (+ x YDELTA)]
                   [else "wrong"])]
    [else "wrong"]))


; LRCD -> LRCD
(define (main x)
  (big-bang x
    [to-draw show]
    [on-tick fly 1]
    [on-key launch]))

; (main "resting")