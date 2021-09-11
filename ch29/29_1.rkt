;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 29_1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(define cyclic-graph
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

; A Node is a Symbol.
; A Graph is [List-of Edge]
; An Edge is (list Node [List-of Node])
; ex471
;Node Graph -> [List-of Node]
(define (neighbors n g)
  (second (assq n g)))

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (foldl (lambda (node result) (if (cons? result)
                                   result
                                   (find-path node D G)))
         #false
         lo-Os))

(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)

; Graph -> Boolean
(define (test-on-all-nodes g)
  (local ((define all-nodes (map first g)))
    (for*/and ([i all-nodes] [j all-nodes]) (match (find-path i j g)
                                              [(? cons?) #t]
                                              [else #f]))))

(define (abstract-find-path origination destination G neighbors)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

;=======================================================================
; ex476
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(define (fsm-match? an-fsm a-string)
  (local ((define lo1s (explode a-string))
          (define output (iter-fsm an-fsm lo1s)))
    (equal? output (fsm-final an-fsm))))

; FSM [List-of 1String] -> FSM-State
(define (iter-fsm an-fsm lo1s)
  (foldl (lambda (char current)
           (next-state (fsm-transitions an-fsm) current char))
         (fsm-initial an-fsm)
         lo1s))

; [List-of Transition] FSM-State 1String -> FSM-State
(define (next-state transitions current c)
  (cond
    [(empty? transitions) current]
    [else (local ((define t (first transitions))
                  (define cur (transition-current t))
                  (define key (transition-key t))
                  (define next (transition-next t)))
            (if (and (equal? key c)
                     (equal? cur current))
                next
                (next-state (rest transitions) current c)))]))

(define l
  (list (make-transition "AA" "a" "BC")
        (make-transition "BC" "b" "BC")
        (make-transition "BC" "c" "BC")
        (make-transition "BC" "d" "DD")))

(next-state l "BC" "d")



                         