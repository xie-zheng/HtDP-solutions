;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname text_editor) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

;-------------------------
; constants 
(define WIDTH 200)
(define HEIGHT 20)
(define FONTSIZE 16)
(define CURSORWIDTH 1)

(define MAXLEN (/ WIDTH FONTSIZE))
(define BG (empty-scene WIDTH HEIGHT))
;-------------------------


; Editor -> Image
; consume an editor and produce the image of text
(define (render-text ed)
  (beside (text (editor-pre ed) FONTSIZE "black")               
          (rectangle CURSORWIDTH FONTSIZE "solid" "red")
          (text (editor-post ed) FONTSIZE "black")))                               


; Editor -> Image
; consume an editor and produce an image
(define (render ed)
  (overlay/align "left" "center"
                (render-text ed)
                BG))
  

;  ----------
; | 辅助函数 |
;  ----------
; String -> String
; 取字符串首个字符
(check-expect (string-first "hello") "h")
(define (string-first s)
  (if (= (string-length s) 0)
      s
      (substring s 0 1)))

; String -> String
; 取字符串末尾字符
(check-expect (string-last "hello") "o")
(define (string-last s)
  (if (= (string-length s) 0)
      s
      (substring s (- (string-length s) 1))))

; String -> String
; 删去字符串首个字符
(check-expect (string-rest "hello") "ello")
(define (string-rest s)
  (if (= (string-length s) 0)
      s
      (substring s 1)))

; String -> String
; 删去字符串末尾字符
(check-expect (string-remove-last "hello") "hell")
(define (string-remove-last s)
  (if (= (string-length s ) 0)
      s
      (substring s 0 (- (string-length s) 1))))


; 用于测试的结构editor示例
(define test-editor
  (make-editor "hello " "world"))

;  --------
; | 子函数 |
;  --------
; Editor KeyEvent -> Editor
; 把ke添加到ed的pre中
(check-expect (add-editor test-editor " ")
              (make-editor "hello  " "world"))
(define (add-editor ed ke)
  (if (>= (image-width (render-text ed)) WIDTH) ; 防止输入内容过长超过文本框宽度
      ed
      (make-editor
       (string-append (editor-pre ed) ke)
       (editor-post ed))))

; Editor -> Editor
; 删去ed中pre的最后一个字符
(check-expect (delete-editor test-editor)
              (make-editor "hello" "world"))
(define (delete-editor ed)
  (make-editor
   (string-remove-last (editor-pre ed))
   (editor-post ed)))


; 把判别长度为0的条件从这里去掉是否不太合适？
; Editor -> Editor
; 将光标向左移动一格
(check-expect (left-shift test-editor)
              (make-editor "hello" " world"))
(define (left-shift ed)
  (if (= (string-length (editor-pre ed)) 0)
      ed
      (make-editor
       (string-remove-last (editor-pre ed))
       (string-append (string-last (editor-pre ed))
                      (editor-post ed)))))

; Editor -> Editor
; 将光标向右移动一格
(check-expect (right-shift test-editor)
              (make-editor "hello w" "orld"))
(define (right-shift ed)
  (if (= (string-length (editor-post ed)) 0)
      ed
      (make-editor
       (string-append (editor-pre ed)
                      (string-first (editor-post ed)))
       (string-rest (editor-post ed)))))


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

(define (run pre)
  (big-bang (make-editor pre "")
    [to-draw render]
    [on-key edit]))