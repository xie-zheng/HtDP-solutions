;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 18_5) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; Tree A
(define a000 (make-node 10 'a000 NONE NONE))
(define a001 (make-node 24 'a001 NONE NONE))
(define a111 (make-node 99 'a111 NONE NONE))

(define a00 (make-node 15 'a00 a000 a001))
(define a10 (make-node 77 'a10 NONE NONE))
(define a11 (make-node 95 'a11 NONE a111))

(define a0 (make-node 29 'a0 a00 NONE))
(define a1 (make-node 89 'a1 a10 a11))

(define a (make-node 63 'a a0 a1))
;=======================================================================

; ex322
; BT N -> Bool
(define (contains-bt? tree num)
  (cond
    [(no-info? tree) #f]
    [else (or (= num (node-ssn tree))
              (contains-bt? (node-left tree) num)
              (contains-bt? (node-right tree) num))]))
(check-expect (contains-bt? a 77) #t)
(check-expect (contains-bt? a 1) #f)

; BT N -> Symbol/#f
(define (search-bt? tree num)
  (cond
    [(no-info? tree) #f]
    [else (if (= num (node-ssn tree))
              (node-name tree)
              (local (; search result
                      (define l (search-bt? (node-left tree) num))
                      (define r (search-bt? (node-right tree) num)))
                (if (symbol? l) l r)))]))
(check-expect (search-bt? a 1) #f)
(check-expect (search-bt? a 77) 'a10)



; ex324
; BT -> [List-of N]
(define (inorder tree)
  (cond
    [(no-info? tree) '()]
    [else (append (inorder (node-left tree))
                  (list (node-ssn tree))
                  (inorder (node-right tree)))]))
(check-expect (inorder a) '(10 15 24 29 63 77 89 95 99))



; ex325
(define (search-bst tree num)
   (cond
     [(no-info? tree) #f]
     [else (cond
             [(> num (node-ssn tree)) (search-bst (node-right tree) num)]
             [(< num (node-ssn tree)) (search-bst (node-left tree) num)]
             [else (node-name tree)])]))
(check-expect (search-bst a 77) 'a10)



; ex326
; BST Number Symbol -> BST
(define (create-bst tree num name)
  (cond
    [(no-info? tree) (make-node num name NONE NONE)]
    [else (cond
            [(> num (node-ssn tree))
             (make-node (node-ssn tree)
                        (node-name tree)
                        (create-bst (node-left tree) num name)
                        (node-right tree))]
            [(< num (node-ssn tree))
             (make-node (node-ssn tree)
                        (node-name tree)
                        (node-left tree)
                        (create-bst (node-right tree) num name))]
            [else
             (make-node (node-ssn tree)
                        name
                        (node-left tree)
                        (node-right tree))])]))

; [List-of [List Number Symbol]] -> BST
(define (create-bst-from-list l)
  (foldr (lambda (l tree) (create-bst tree (first l) (second l)))
         NONE
         l))
; BT -> Boolean
(define (order? tree)
  (cond
    [(no-info? tree) #t]
    [else
     (and (< (num-or-f (tree-max (node-left tree))
                       (- (node-ssn tree) 1))
             (node-ssn tree)
             (num-or-f (tree-min (node-right tree))
                       (+ (node-ssn tree) 1)))
          (order? (node-left tree))
          (order? (node-right tree)))]))

(define (num-or-f x y)
  (if (equal? x #f) y x))

(define (tree-min tree)
  (cond
    [(no-info? tree) #f]
    [else (if (equal? (node-left tree) NONE)
              (node-ssn tree)
              (tree-min (node-left tree)))]))
(define (tree-max tree)
  (cond
    [(no-info? tree) #f]
    [else (if (equal? (node-right tree) NONE)
              (node-ssn tree)
              (tree-max (node-right tree)))]))

(check-expect (order? a) #t)

