(define make-node 
  (lambda (node-value children)
        (cons node-value children)))

(define get-node
  (lambda (tree)
    (car tree)))

(define get-key
  (lambda (node)
    (cond
      [(null? node) `()]
      [else (caar node)])))

(define get-value
  (lambda (node)
    (cadar node)))

(define children
  (lambda (node)
    (cdr node)))

(define left-child
  (lambda (tree)
    (cond
      [(null? (children tree)) `()]
      [else (car (children tree))])))

(define lst-of-null-ele?
  (lambda (lst)
    (cond
      [(null? lst) #t]
      [else (and (null? (car lst)) (lst-of-null-ele? (cdr lst)))])))

(define isLeaf?
  (lambda (tree)
    (if (lst-of-null-ele? (cdr tree))
        #t
        #f)))

(define right-child
  (lambda (tree)
    (cond
      [(null? (children tree)) `()]
      [(null? (cdr (children tree))) `()]
      [else (car (cdr (children tree)))])))

(define size
  (lambda (tree)
    (cond
      [(null? tree) 0]
      [else (+ 1 (size (left-child tree)) (size (right-child tree)))])))

(define traversal
  (lambda (tree)
    (cond
      [(null? tree) `()]
      [else
       (cons (get-node tree) (list (pre-order-traversal (left-child tree)) (pre-order-traversal (right-child tree))))])))

;Find the k-th biggest element in the bst
(define find-kth-biggest-ele
  (lambda (tree k)
    (call/cc
      (lambda (call-cc)
        (letrec ([repeat-find
                  (lambda (tree)
                    (cond
                      [(null? tree) `()]
                      [else 
                       (begin
                        (repeat-find (right-child tree))
                        (if (= k 1) (call-cc (get-key tree)) (set! k (sub1 k)))
                        (repeat-find (left-child tree)))]))])
          (repeat-find tree))))))

(define in-order-traversal
  (lambda (tree)
    (cond
      [(null? tree) `()]
      [else
       (list (in-order-traversal (left-child tree)) (get-node tree) (in-order-traversal (right-child tree)))])))

(define get-first-ele-inorder-traversal
  (lambda (tree)
    (call/cc
      (lambda (k)
        (let recur-traversal ([tree tree])
          (cond
            [(null? tree) `()]
            [else 
             (begin
               (recur-traversal (left-child tree))
               (if (not (null? (get-node tree)))
                   (k (get-node tree))))]))))))

; datum is a list contains (list key value)
(define insert
  (lambda (tree node-value)
    (letrec ([partial-insert
              (lambda (tree node-value)
                (cond 
                  [(null? tree) (make-node node-value (list `() `()))]
                  [(> (car node-value) (get-key tree)) (cons (get-node tree) (list (left-child tree) (partial-insert (right-child tree) node-value)))]
                  [(< (car node-value) (get-key tree)) (cons (get-node tree) (list (partial-insert (left-child tree) node-value) (right-child tree)))]))])
      (partial-insert tree node-value))))

(define search
  (lambda (tree key)
    (cond
      [(null? tree) `()]
      [(> key (get-key tree)) (search (right-child tree) key)]
      [(< key (get-key tree)) (search (left-child tree) key)]
      [else tree])))

(define delete
  (lambda (tree key)
    (cond
      [(null? tree) `()]
      [(> key (get-key tree)) (cons (get-node tree) (list (left-child tree) (delete (right-child tree) key)))]
      [(< key (get-key tree)) (cons (get-node tree) (list (delete (left-child tree) key) (right-child tree)))]
      [else
       (cond
         [(isLeaf? tree) `()]
         [(and (not (null? (left-child tree))) (not (null? (right-child tree))))
          (let ([root (get-first-ele-inorder-traversal (right-child tree))])
            (make-node root (list (left-child tree) (delete (right-child tree) (car root)))))]
         [(null? (left-child tree)) (right-child tree)]
         [(null? (right-child tree)) (left-child tree)])])))

(define insert-lst-of-node
  (lambda (tree lst-of-node)
    (if (null? lst-of-node)
        tree
        (insert-lst-of-node (insert tree (car lst-of-node)) (cdr lst-of-node)))))

