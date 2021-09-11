;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 20_3) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require htdp/dir)

; [List-of Number] -> N
(define (sum l)
  (foldr + 0 l))

(define (how-many d)
  (+ (length (dir-files d))
     (sum (map how-many (dir-dirs d)))))

(define (find? d name)
  (or (ormap (lambda (f) (string=? name (file-name f))) (dir-files d))
      (ormap (lambda (x) (find? x name)) (dir-dirs d))))

(define (ls-raw dir)
  (append (for/list ([d (dir-dirs dir)]) (dir-name d))
          (for/list ([f (dir-files dir)]) (file-name f))))

(define (ls dir)
  (write-file
   'stdout
   (format (foldr (lambda (x y) (string-append x "\n" y))
                  ""
                  (sort (ls-raw dir) string>?)))))

(define (du dir)
  (+ (for/sum ([f (dir-files dir)]) (file-size f))
     (for/sum ([d (dir-dirs dir)]) (du d))))

(define (find d f)
  (if (find? d f)
      (get-path d f)
      #f))

; [List-of dir] -> Path
(define (get-path d f)
  (if (ormap (lambda (x) (string=? f (file-name x))) (dir-files d))
      (list (dir-name d) f) ; file in current dir
      (cons (dir-name d) (check-dirs (dir-dirs d) f)))) ; file in subdir


(define (check-dirs lod f)
  (cond
    [(empty? lod) '()]
    [else (if (find? (first lod) f)
              (get-path (first lod) f)
              (check-dirs (rest lod) f))]))

(define W (create-dir "E:\\rack")) ; on Windows 



; Dir String -> or/c [List-of Path] #false
(define (find-all d f)
  (if (find? d f)
      (get-all d f)
      #f))

; Dir String -> [List-of Path]
(define (get-all d f)
  (local (; exist in here
          (define dirname (dir-name d))
          ; path for current directory
          (define here (if (file-in? f (dir-files d))
                           (list dirname f)
                           #f))
          ; path in subdirectory
          (define subfind
            (map (lambda (dir) (find-all dir f)) (dir-dirs d)))
          ; filter #f
          (define false-removed
            (filter (lambda (x) (not (equal? x #f))) subfind))
          ; append current directory in paths
          (define paths
            (map (lambda (l) (cons dirname l)) false-removed)))
    ; - IN -
    (if (equal? here #f)
        paths
        (if (empty? paths)
            here
            (cons here paths)))))


;
(define (ls-r d)
  (add-dname (dir-name d)
             (append
              (for/list ([file (dir-files d)]) (list (file-name file)))
              (handle-dirs (dir-dirs d)))))

; [List-of Dir] -> [List-of [List-of Path]]
(define (handle-dirs lod)
  (foldr append '() 
         (filter (lambda (dir) (not (empty? dir)))
                 (for/list ([dir lod]) (ls-r dir)))))

(define (add-dname name lop)
  (for/list ([path lop]) (cons name path)))
            

(define (new-find-all d f)
  (filter (lambda (path) (string=? (last path) f))
          (ls-r d)))

; [X] [NeList-of X] -> X
(define (last l)
  (match l
    [(cons lst '()) lst]
    [(cons fst rst) (last rst)]))

(define (file-in? f files)
  (ormap (lambda (x) (string=? f (file-name x))) files))

(get-all W "a.txt")