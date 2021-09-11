;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12_5) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; Constants
(define WORM-R 10)
(define WORM-D (* WORM-R 2))
(define WORM (circle WORM-R "solid" "red"))
(define FOOD (circle WORM-R "solid" "green"))

(define MAX 20)
(define W MAX)
(define H MAX)
(define WIDTH (* WORM-D W))
(define HEIGHT (* WORM-D H))
(define MT (empty-scene WIDTH HEIGHT))

; A worm is a struct:
;   (make-worm direction Posn)
(define-struct worm [d pos])

; A world is a struct:
;  (make-world Worm Posn
(define-struct world [worm food])

; A list-of-worms is one of
; - (cons Worm '())
; - (cons Worm List-of-worms)
; A real worm is a list of worm segments

;----------------------
(define (main-worm w)
  (big-bang w
    [on-tick   tock 1]
    [on-key    kehandler]
    [to-draw      render]
    [stop-when hit? last-scene]
    [state #t]))

;=======================================================================
(define (tock w)
  (cond
    [(eat? w) (grow w)] ; hit food? -> worm grow & gen new food
    [else (make-world (forward (world-worm w)) ; just move
                      (world-food w))]))

(define (eat? w)
  (equal? (world-food w)
          (worm-pos (first (world-worm w)))))

(define (grow w)
  (make-world (worm-eat (world-worm w))
              (food-create (world-food w))))

(define (worm-eat worm)
  (cons (move (first worm)) worm))

; List-of-worms -> List-of-worms
; move worm forward by removing last element and add a new element at head
(define (forward low)
  (cons (move (first low))
        (cut low)))

; List-of-worms -> List-of-worms
; cut tail
(define (cut low)
  (cond
    [(empty? low) '()]
    [(empty? (rest low)) '()]
    [else (cons (first low)
                (cut (rest low)))]))

; Worm -> Worm
; move worm's logical position
(define (move w)
  (make-worm (worm-d w)
             (cond
               [(string=? (worm-d w) "left")  (posn-move (worm-pos w) -1  0)]
               [(string=? (worm-d w) "right") (posn-move (worm-pos w)  1  0)]
               [(string=? (worm-d w) "up")    (posn-move (worm-pos w)  0 -1)]
               [(string=? (worm-d w) "down")  (posn-move (worm-pos w)  0  1)])))

; Posn -> Posn
; move posn by dx dy
(define (posn-move p dx dy)
  (make-posn (+ (posn-x p) dx)
             (+ (posn-y p) dy)))



;=======================================================================
; World KeyEvent -> World
(define (kehandler w ke)
  (make-world (kehandler-worm (world-worm w) ke)
              (world-food w)))

; List-of-Worm KeyEvent -> List-of-Worm
; only change direction of head
(define (kehandler-worm low ke)
  (cons (kehandler-seg (first low) ke)
        (rest low)))

; Worm KeyEvent -> Worm
(define (kehandler-seg w ke)
  (cond
    [(member? ke (list "left" "right" "up" "down")) (changed w ke)]
    [else w]))

; Worm String -> Worm
(define (changed w d)
  (make-worm d (worm-pos w)))



;=======================================================================
; World -> Image
(define (render w)
  (render-food (world-food w)
               (render-worm (world-worm w))))

; Posn Image -> Image
(define (render-food f bg)
  (place-image FOOD
               (get-pos (posn-x f)) (get-pos (posn-y f))
               bg))

; List-of-Worm Image -> Image
(define (render-worm low)
  (cond
    [(empty? low) MT]
    [else (render-seg (first low) (render-worm (rest low)))]))

; Worm Image -> Image
(define (render-seg w bg)
  (place-image WORM
               (get-pos (posn-x (worm-pos w))) (get-pos (posn-y (worm-pos w)))
               bg))

; Number -> Number
(define (get-pos x)
  (+ (* x WORM-D) WORM-R))



; Worm -> Image
(define (last-scene w)
  (place-image (if (hit-wall? (world-worm w))
                   (text "worm hit border" (* WORM-R 4) "red")
                   (text "worm run into itself" (* WORM-R 4) "red"))
               (/ WIDTH 2)
               (* HEIGHT (/ 3 4))
               (render w)))



;=======================================================================
; List-of-worms -> Bool
(define (hit-self? low)
  (pos-in? (rest low) (worm-pos (first low))))

; List-of-worms Posn -> Bool
(define (pos-in? low pos)
  (cond
    [(empty? low) #false]
    [else (if (equal? (worm-pos (first low)) pos)
              #true
              (pos-in? (rest low) pos))]))

; List-of-worms -> Bool
(define (hit-wall? low)
  (cond
    [(empty? low) #false]
    [else (or (hit-wall?-seg (first low))
              (hit-wall? (rest low)))]))

; Worm -> Bool
(define (hit-wall?-seg w)
  (not (and (<= 0 (posn-x (worm-pos w)) (- W 1))
            (<= 0 (posn-y (worm-pos w)) (- H 1)))))

; List-of-worms -> Bool
(define (hit? w)
  (or (hit-self? (world-worm w))
      (hit-wall? (world-worm w))))
;=======================================================================
; Posn -> Posn 
; ???
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
     p (make-posn (random MAX) (random MAX))))
 
; Posn Posn -> Posn 
; generative recursion 
; ???
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))


;=======================================================================
(define tworm (list (make-worm "right" (make-posn 5 2))
                    (make-worm "right" (make-posn 4 2))
                    (make-worm "right" (make-posn 3 2))
                    (make-worm "right" (make-posn 2 2))
                    (make-worm "right" (make-posn 1 2))))

(define tw (make-world tworm (food-create (make-posn 1 1))))
