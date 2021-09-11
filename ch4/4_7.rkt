;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 4_7) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define LOCKED "locked") ; A DoorState is one of:
(define CLOSED "closed") ; – LOCKED
(define OPEN "open")     ; – CLOSED
                         ; – OPEN


; DoorState -> DoorState
; close the open door after every tick
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)

(define (door-closer ds)
  (cond
    [(equal? ds OPEN) CLOSED]
    [else ds]))


; DoorState KeyEvent -> DoorState
; lock the door when press "l"
; unlock the door when press "u"
; open the door when press space
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)

(define (door-action ds ke)
  (cond
    [(equal? ke " ") (if (equal? ds CLOSED) OPEN ds)]
    [(equal? ke "u") (if (equal? ds LOCKED) CLOSED ds)]
    [(equal? ke "l") (if (equal? ds CLOSED) LOCKED ds)]
    [else ds]))


(define (door-renderer ds)
  (cond
    [(equal? ds LOCKED) (text "locked" 30 "red")]
    [(equal? ds OPEN) (text "open" 30 "green")]
    [(equal? ds CLOSED) (text "closed" 30 "black")]))


(define (main ds)
  (big-bang ds
    [on-tick door-closer 2]
    [on-key door-action]
    [to-draw door-renderer]))
   




