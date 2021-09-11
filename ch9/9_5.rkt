;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9_5) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define HEIGHT 220) ; distances in terms of pixels 
(define WIDTH 30)
(define XSHOTS (/ WIDTH 3))
 
; graphical constants 
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "green")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))
(define SHOT (rectangle 2 10 "solid" "red"))



; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot



; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))


; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cond
            [(> (sub1 (first w)) 0) (cons (sub1 (first w)) (tock (rest w)))]
            [else (tock (rest w))])]))

(check-expect (tock '()) '())
(check-expect (tock (cons 5 '())) (cons 4 '()))


; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))





; ex159
;============================================================
; HELPER FUNCTIONS
; ----------------          
; N, Image -> Image
; produces a column—a vertical arrangement—of n copies of img.
(define (col n img)
  (cond
    [(= n 1) img]
    [(positive? n) (above img (col (sub1 n) img))]))

; N, Image -> Image
; produces a row vertical arrangement—of n copies of img.
(define (row n img)
  (cond
    [(= n 1) img]
    [(positive? n) (beside img (row (sub1 n) img))]))
;=============================================================

;=============================================================
; CONSTANTS
;
(define box (rectangle 10 10 "outline" "black"))
; lecture hall
(define BOX (rectangle 10 10 "outline" "black"))
(define RECT (row 10 (col 20 BOX)))
(define WIDTH1 (image-width RECT))
(define HEIGHT1 (image-height RECT))
(define HALL (place-image RECT
                          (/ (image-width RECT) 2)
                          (/ (image-height RECT) 2)
                          (empty-scene (image-width RECT) (image-height RECT))))
(define DOT (circle 2 "solid" "red"))

; main function
(define (riots n)
  (big-bang (gen-attacks n)
    [on-tick tocker]
    [to-draw render]
    [stop-when empty?]))

(define (gen-attacks n)
  (cond
    [(zero? n) '()]
    [else (cons (make-posn (random WIDTH1) (random HEIGHT1))
                (gen-attacks (sub1 n)))]))


(define (tocker attacks)
  (cond
    [(empty? attacks) '()]
    [else (cond
            [(out? (first attacks)) (tocker (rest attacks))]
            [else (cons (move-ballon (first attacks))
                        (tocker (rest attacks)))])]))

(define (out? posn)
  (> (posn-y posn) HEIGHT1))

(define (move-ballon posn)
  (make-posn (posn-x posn)
             (add1 (posn-y posn))))

(define (render attacks)
  (add-ballons attacks HALL))
; List of Posn, Image -> Image
; THe function consumes a list of Posn whose coordinates
;   fit into the dimensions of the lecture hall.
;   It produces an image of the lecture hall with red dots
;   added as specified by the Posns.
(define (add-ballons posns hall)
  (cond
    [(empty? posns) hall]
    [else (add-ballons (rest posns)
                       (add-ballon (first posns) hall))]))

; Posn, Image -> Image
; produces an image of the leture hall with red dots
; added as specified by the Posn
(define (add-ballon posn hall)
  (place-image DOT
               (posn-x posn)
               (posn-y posn)
               hall))