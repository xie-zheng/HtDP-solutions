;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 3_2) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; String -> 1String
; extract the first character from a string
; given:
;   "hello world"
; expect:
;   "h"
(define (string-first str)
  (substring str 0 1))


; String -> 1String
; extracts the last character from a non-empty string
; given:
;   "hello world"
; expect:
;   "d"
(define (Stirng-last str)
  (substring str (- (string-length str) 1)))


; Image -> Number
; counts the number of pixels in a given image
; given:
;   (rectangle 10 10 "solid" "red")
; expect:
;   100
(define (image-area image)
  (* (image-width image)
     (image-height image)))


; String -> String
; produce a string with the first character removed
; given:
;   "hello world"
; expect:
;   "ello world"
(define (string-rest str)
  (substring str 1))


; String -> String
; produce a string with the last character removed
; given:
;   "hello world"
; expect:
;   "hello worl"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))


