;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12_6) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; ------------------
; Physical constants
; ------------------
(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 10) ; # of blocks, vertically
(define SIZE 10)  ; blocks are squares
(define MID (/ SIZE 2)) ; to locate blocks
(define SCENE-SIZE (* WIDTH SIZE))



; -------------------
; Graphical constants
; -------------------
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))
(define BG (empty-scene SCENE-SIZE SCENE-SIZE))



; ------------------
; Struct definitions
; ------------------

; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
(define-struct tetris [block landscape])

; A Block is a structure:
;   (make-block N N)
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting
(define-struct block [x y])

; ----
; main
; ----
(define (tetris-main tetris clock-rate)
  (big-bang tetris
    [on-tick game clock-rate]
    [on-key kehandler]
    [to-draw render]
    [stop-when touchtop]))


; ----
; Game
;   handler of most of logics
; ----
(define (game t)
  (cond
    [(land? t) (xiaochu (new-block t))]
    [else (descend t)]))

; Tetris/Block -> Tetris/Block
(define (descend t)
  (cond
    [(tetris? t) (make-tetris (descend (tetris-block t)) (tetris-landscape t))]
    [(block? t) (make-block (block-x t) (+ (block-y t) 1))]))

; Tetris -> Boolean
(define (land? t)
  (or (member? (descend (tetris-block t)) (tetris-landscape t))
      (= (block-y (tetris-block t)) (- HEIGHT 1))))

; Tetris -> Tetris
(define (new-block t)
  (make-tetris (make-block (random WIDTH) 0)
               (cons (tetris-block t) (tetris-landscape t))))

; Tetris -> Boolean
; check whether bottom is full
(define (full? t)
  (= WIDTH (length (level t 0))))

; Tetris Number -> List-of-blocks
; get one level of blocks
(define (level t l)
  (cond
    [(tetris? t) (level (tetris-landscape t) l)]
    [else (cond
            [(empty? t) '()]
            [else (if (= (- HEIGHT (+ l 1)) (block-y (first t)))
                      (cons (first t) (level (rest t) l))
                      (level (rest t) l))])]))
          
; Tetris -> Tetris
(define (xiaochu t)
  (cond
    [(full? t) (descend-all (removelevel t 0))]
    [else t]))

(define (removelevel t l)
  (cond
    [(tetris? t) (make-tetris (tetris-block t) (removelevel (tetris-landscape t) l))]
    [else (cond
            [(empty? t) '()]
            [else (if (= (- HEIGHT (+ 1 l)) (block-y (first t)))
                      (removelevel (rest t) l)
                      (cons (first t) (removelevel (rest t) l)))])]))

(define (descend-all t)
  (cond
    [(tetris? t) (make-tetris (tetris-block t) (descend-all (tetris-landscape t)))]
    [else (cond
            [(empty? t) '()]
            [else (cons (descend (first t)) (descend-all (rest t)))])]))
; ----------------
; helper functions
; ----------------
; block -> Number
; get the x-coordinate of the block
(define (posx b)
  (+ MID (* SIZE (block-x b))))
(check-expect (posx block-landed) MID)

; block -> Number
; get the x-coordinate of the block
(define (posy b)
  (+ MID (* SIZE (block-y b))))
(check-expect (posy block-landed) (+ MID (* SIZE (- HEIGHT 1))))

; ----------------
; KeyEvent Handler
; ----------------
(define DIRECTION (list "left" "right"))
; Tetris KeyEvent -> Tetris
(define (kehandler t ke)
  (cond
    [(member? ke DIRECTION)
     (if (member? (move (tetris-block t) ke) (tetris-landscape t))
         t
         (make-tetris (move (tetris-block t) ke) (tetris-landscape t)))]
     [else t]))

; Block KeyEvent -> Block
(define (move b d)
  (cond
    [(equal? d "left")
     (if (< 0 (block-x b))
         (make-block (- (block-x b) 1) (block-y b))
         b)]
    [(equal? d "right")
     (if (< (block-x b) (- WIDTH 1))
         (make-block (+ (block-x b) 1) (block-y b))
         b)]))


; ---------------
; image rendering
; ---------------
; Block Image -> Image
(define (render-block b bg)
  (place-image BLOCK (posx b) (posy b) bg))

(check-expect (render-block block-landed BG)
              (place-image BLOCK (posx block-landed) (posy block-landed) BG))

; Landscape Image -> Image
(define (render-landscape l bg)
  (cond
    [(empty? l) bg]
    [else (render-block (first l) (render-landscape (rest l) bg))]))

; Tetris -> Image
(define (render t)
  (render-block (tetris-block t) (render-landscape (tetris-landscape t) BG)))


; ---------
; Stop-When
; ---------
(define (touchtop t)
  (cond
    [(tetris? t) (touchtop (tetris-landscape t))]
    [(list? t) (cond
                 [(empty? t) #false]
                 [else (if (= 0 (block-y (first t)))
                           #true
                           (touchtop (rest t)))])]))



(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape0 (list block-on-block block-landed))
(define block-dropping (make-block 2 3))
(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-drop (make-block 2 (- HEIGHT 1)))

