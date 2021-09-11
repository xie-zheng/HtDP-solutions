;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 2_1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; 11
(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

; 12
(define (cvolume l)
  (* l l l))
(define (csurface l)
  (* 6 l l))

; 13
(define (string-first str)
  (substring str 0 1))

; 14
(define (string-last str)
  (substring str (- (string-length str) 1)))

; 15
(define (==> sunny friday)
  (or (not sunny) friday))

; 16
(define (image-area image)
  (* (image-width image)
     (image-height image)))

; 17
(define (image-classify image)
  (cond
    [(> (image-width image) (image-height image)) "wide"]
    [(< (image-width image) (image-height image)) "tall"]
    [else "square"]))

; 18
(define (string-join str1 str2)
  (string-append str1 "_" str2))

; 19
(define (string-insert str i)
  (string-append (substring str 0 i)
                 "_"
                 (substring str i)))

; 20
(define (string-delete str i)
  (if (>= (string-length str) i)
      (string-append (substring str 0 i)
                     (substring str (+ i 1)))
      str))


    
  