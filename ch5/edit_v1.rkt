;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname edit_v1) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define (add-editor ed ke)
  (make-editor
   (string-append (editor-pre ed) ke)
   (editor-post ed)))

(define (delete-editor ed)
  (make-editor
   (remove-pre-last ed)
   (editor-post ed)))

(define (remove-pre-last ed)
  (substring (editor-pre ed)
             0
             (- (string-length (editor-pre ed)) 1)))

(define (append-to-post ed)
  (string-append (substring (editor-pre ed) (- (string-length (editor-pre ed)) 1))
                 (editor-post ed)))

(define (left-shift ed)
  (if (= (string-length (editor-pre ed)) 0)
      ed
      (make-editor
       (remove-pre-last ed)
       (append-to-post ed))))
              
(define (right-shift ed)
  (if (= (string-length (editor-post ed)) 0)
      ed
      (make-editor
       (string-append (editor-pre ed) (substring (editor-post ed) 0 1))
       (substring (editor-post) 1))))

; Editor KeyEvent -> Editor
; add a single-character KeyEvent ke to the end of the pre field of ed
; or deletes the character immediately to the left of the cursor
; when ke is "\b"
(define (edit ed ke)
  (cond
    [(equal? ke "\b") (delete-editor ed)]
    [(equal? ke "\r") ed]
    [(equal? ke "\t") ed]
    [(equal? ke "left") (left-shift ed)]
    [(equal? ke "right") (right-shift ed)]
    [else (add-editor ed ke)])) 