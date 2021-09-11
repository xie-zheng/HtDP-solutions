;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23_7) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)


 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch



(define school-schema
  `(,(make-spec "Name"    string?)
    ,(make-spec "Age"     integer?)
    ,(make-spec "Present" boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema
           school-content))



(define presence-schema
  `(,(make-spec "Present"     boolean?)
    ,(make-spec "Description" string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))




; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
(define (integrity-check db)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define width   (length schema))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (s c) [(spec-predicate s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))

(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
 
; ex404
(define (andmap2 f l1 l2)
  (for/and ([i l1] [j l2]) (f i j)))


(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))
;  Stop! Read this test carefully. What's wrong?
(check-expect (db-content (project school-db '("Name" "Present")))
              projected-content)

; DB [List-of Label] -> DB
; retains a column from db if its label is in labels
(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (spec-label c) labels))
          
          (define mask (map keep? schema))
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask)))
    (make-db (filter keep? schema)
             (map row-project content))))


(define (select db labels predicate)
  (local ((define selected-db
            (make-db (db-schema db)
                     (filter predicate (db-content db)))))
    (project selected-db labels)))

(define (reorder db lol)
  (local ((define labels (map spec-label (db-schema db)))
          (define (index label labels n)
            (if (string=? label (first labels))
                n
                (index label (rest labels) (add1 n))))
          (define orders (map (lambda (x) (index x labels 0)) lol))
            
          (define (row-reorder row)
            (for/list ([order orders]) (list-ref row order))))
    (make-db (row-reorder (db-schema db))
             (map row-reorder (db-content db)))))
            

(define (db-union db1 db2)
  (make-db (db-schema db1)
           (foldr (lambda (x y) (if (member? x y) y (cons x y)))
                  db2
                  db1)))

(define (last row)
  (cond
    [(empty? (rest row)) (first row)]
    [else (last (rest row))]))

(define (join db1 db2)
  (local ((define content1 (db-content db1))
          (define content2 (db-content db2))

          (define (join-row r1 r2)
            (cond
              [(empty? (rest r1)) (rest r2)]
              [else (cons (first r1)
                          (join-row (rest r1) r2))]))
          (define (search-match tail content)
            (cond
              [(empty? content) '()]
              [else (if (equal? tail (first (first content)))
                        (first content)
                        (search-match tail (rest content)))])))
    (make-db (join-row (db-schema db1) (db-schema db2))
             (map (lambda (row) (join-row row (search-match (last row) content2)))  content1))))

(define what-db
  (make-db
   `(,(make-spec "Present" boolean?)
     ,(make-spec "Description" string?))
   `((#true "presence")
     (#false "absence"))))
             
