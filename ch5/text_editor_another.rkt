;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname text_editor_another) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;  ----------
; | 结构定义 |
;  ----------
(define-struct editor [str index])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s i) describes an editor
; whose visible text is s with the cursor displayed at position i


;  ------
; | 常量 |
;  ------
(define WIDTH 400)
(define HEIGHT 40)
(define FONTSIZE 32)
(define CURSORWIDTH 1)

(define BG (empty-scene WIDTH HEIGHT))



; Editor -> Image
; 仅渲染文本和光标
(define (render-text ed)
  (beside (text (substring (editor-str ed) 0 (editor-index ed)) FONTSIZE "black")
          (rectangle CURSORWIDTH FONTSIZE "solid" "red")
          (text (substring (editor-str ed) (editor-index ed)) FONTSIZE "black")))

; Editor -> Image
; 渲染整个文本编辑器
(define (render ed)
  (overlay/align "left" "center"
                (render-text ed)
                BG))
  

;  ----------
; | 辅助函数 |
;  ----------
; String Number 1String -> String
; 将字符s插入到字符串str中的i位置
(check-expect (string-insert-ith "helloworld" 5 " ")
              "hello world")
(define (string-insert-ith str i s)
  (string-append (substring str 0 i)
                 s
                 (substring str i)))

; String Number -> String
; 删去字符串str中i位置的字符
(check-expect (string-delete-ith "hello world" 6)
              "helloworld")
(define (string-delete-ith str i)
  (if (= i 0)
      str
      (string-append (substring str 0 (- i 1))
                     (substring str i))))


; 用于测试的结构editor示例
(define test-editor
  (make-editor "hello world" 6))

;  --------
; | 子函数 |
;  --------
; Editor KeyEvent -> Editor
; 把ke插入到str中的index位置
(check-expect (add-editor test-editor " ")
              (make-editor "hello  world" 7))
(define (add-editor ed ke)
  (if (>= (image-width (render-text ed)) WIDTH) ; 防止输入内容过长超过文本框宽度
      ed
      (make-editor
       (string-insert-ith (editor-str ed) (editor-index ed) ke)
       (+ (editor-index ed) (string-length ke)))))


; Editor -> Editor
; 删去str中index位置的字符
(check-expect (delete-editor test-editor)
              (make-editor "helloworld" 5))
(define (delete-editor ed)
  (make-editor (string-delete-ith (editor-str ed) (editor-index ed))
               (max (- (editor-index ed) 1) 0)))


; 把判别长度为0的条件从这里去掉是否不太合适？
; Editor -> Editor
; 将光标向左移动一格
(check-expect (left-shift test-editor)
              (make-editor "hello world" 5))
(define (left-shift ed)
  (make-editor (editor-str ed)
               (max (- (editor-index ed) 1) 0)))

; Editor -> Editor
; 将光标向右移动一格
(check-expect (right-shift test-editor)
              (make-editor "hello world" 7))
(define (right-shift ed)
  (make-editor (editor-str ed)
               (min (string-length (editor-str ed))
                    (+ (editor-index ed) 1))))


; Editor KeyEvent -> Editor
; add a single-character KeyEvent ke to the end of the pre field of ed
; or deletes the character immediately to the left of the cursor
; when ke is "\b"
(define (edit ed ke)
  (cond
    [(equal? ke "\b") (delete-editor ed)]
    [(string-whitespace? ke) ed]
    [(equal? ke "left") (left-shift ed)]
    [(equal? ke "right") (right-shift ed)]
    [else (add-editor ed ke)]))

(define (run pre)
  (big-bang (make-editor pre (string-length pre))
    [to-draw render]
    [on-key edit]))