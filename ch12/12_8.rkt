;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12_8) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))


; ex226
; FSM-State FSM-State -> Boolean
; check whether two states are equal
(define (state=? s1 s2)
  (equal? s1 s2))

; ex227
; BW(BlackWhite?) Machine is an FSM that flips from black to white and
; back to black for every key event
; A BW is one of:
;   - '()
;   - (cons Transition BW)


(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image 
(define (render-state.v2 s)
  empty-image)
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(define (find-next-state.v2 cs ke)
  cs)

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))
 
(check-expect (state-as-colored-square
               (make-fs ex230 "red"))
              (square 100 "solid" "red"))




;(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)
;(define ex109 (make-fs
;               (list
;                (make-ktransition "red" "a" "yellow")
;                (make-ktransition "yellow" "b" "yellow")
;                (make-ktransition "yellow" "c" "yellow")
;                (make-ktransition "yellow" "d" "green"))))

; ex230

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

(define ex230
  (make-fsm "red"
            (list
             (make-transition "red" "a" "yellow")
             (make-transition "yellow" "b" "yellow")
             (make-transition "yellow" "c" "yellow")
             (make-transition "yellow" "d" "green"))
            "green"))

(define (fsm-simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))


; SimulationState.v3 KeyEvent -> SimulationState.v3
; finds the next state from an-fsm and ke
(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fsm-transitions (fs-fsm an-fsm)) (fs-current an-fsm) ke)))

(check-expect
 (find-next-state (make-fs ex230 "red") "a")
 (make-fs ex230 "yellow"))
(check-expect
 (find-next-state (make-fs ex230 "yellow") "b")
 (make-fs ex230 "yellow"))
(check-expect
 (find-next-state (make-fs ex230 "yellow") "d")
 (make-fs ex230 "green"))



; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(define (find transitions current ke)
  (cond
    [(empty? transitions)
     (error (string-append "not found: " current))]
    [else
     (if (and (equal? (transition-current (first transitions))
                      current)
              (equal? (transition-key (first transitions))
                      ke))
         (transition-next (first transitions))
         (find (rest transitions) current ke))]))

;(check-expect (find fsm-traffic "red") "green")
;(check-expect (find fsm-traffic "green") "yellow")
;(check-error (find fsm-traffic "black")
;             "not found: black")

(fsm-simulate ex230 "red")
