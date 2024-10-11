#lang typed/racket

(struct node ([value : Integer] [left : BinaryTree] [right : BinaryTree]))
(define-type BinaryTree (U node Null))

(: sum-tree (BinaryTree -> Integer))
(define (sum-tree tree)
  (if (null? tree)
      0
      (+ (node-value tree)
         (sum-tree (node-left tree))
         (sum-tree (node-right tree)))))

(define my-tree
  (node 5
        (node 3
              (node 1 null null)
              (node 4 null null))
        (node 8
              (node 6 null null)
              (node 10 null null))))

(: tree-height (BinaryTree -> Integer))
(define (tree-height tree)
  (if (null? tree)
      0
      (+
       1
       (max (tree-height (node-left tree))
            (tree-height (node-right tree))))))


(sum-tree my-tree) 