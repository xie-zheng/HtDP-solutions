;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
(define (to-num item)
  (cond
    [(string? item) (string-length item)]
    [(image? item) (* (image-width item)
                      (image-height item))]
    [(number? item) (if (rational? item)
                        (if (<= item 0) item (- item 1))
                        "不支持复数")]
    [(boolean? item) (if item 10 20)]
    [else "Don't know."]))