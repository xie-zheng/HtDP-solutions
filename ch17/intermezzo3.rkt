;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo3) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define (and-map l f)
  (cond
    [(empty? (rest l)) (f (first l))]
    [else (if (equal? (f (first l)) #f)
              #f
              (and-map (rest l) f))]))

(define (or-map l f)
  (cond
    [(empty? (rest l)) (f (first l))]
    [else (if (equal? (f (first l)) #f)
              (or-map (rest l) f)
              (f (first l)))]))

; ex306
(for/list ([i (in-range 0 10 1)]) i)
(for/list ([i (in-range 1 11 1)]) i)
(for/list ([i (in-range 1 11 1)]) (/ 1 i))

(define (diag n)
  (for/list ([i n]) (for/list ([j n]) (if (= i j)
                                          1
                                          0))))

(define (find-name name lon)
  (for/or ([n lon]) (if (string-contains? name n)
                        n
                        #f)))
(check-expect (find-name "he" '("how" "are" "h" "hello" "he"))
              "hello")

(define (limit-length lon size)
  (for/and ([n lon]) (<= (string-length n) size)))
(check-expect (limit-length '("how" "are" "h" "hello" "he") 1)
              #f)
(check-expect (limit-length '("how" "are" "h" "hello" "he") 5)
              #t)




(define-struct layer [color doll])
; An RD.v2 (short for Russian doll) is one of: 
; – "doll"
; – (make-layer String RD.v2)
(define (depth d)
  (match d
    [(layer color doll) (+ 1 (depth doll))]
    [str 1]))

(define (move-right lop n)
  (match lop
    [(list (posn x y)) (list (make-posn (+ x n) y))]
    [(cons (posn x y) rest) (cons (make-posn (+ x n) y) (move-right rest n))]))


(define (move-right lop delta-x)
  (cond
    [(empty? lop) '()]
    [else (cons (make-posn (+ delta-x (posn-x (first lop)))
                           (posn-y (first lop)))
                (move-right (rest lop) delta-x))]))