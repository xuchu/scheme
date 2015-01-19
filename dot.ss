(load "binary-search-tree.ss")


(define tree `())
(set! tree (insert-lst-of-node tree `((44 44) (17 17) (28 28) (29 29) (88 88) (65 65) (97 97)
                                            (54 54) (82 82) (76 76) (80 80) (78 78))))

; (lst-to-string '("hello" "world")) => "helloworld"
(define lst-to-string
  (lambda (lst)
    (apply string-append lst)))

(define plot-trees
  (lambda (trees)
    (cond
      [(null? trees) `()]
      [else
       (append
         (plot-tree (car trees))
         (plot-trees (cdr trees)))])))

;; can plot whatever tree
(define plot-tree
  (lambda (tree)
    (cond 
      [(null? tree) `()]
      [(letrec ([child-trees (children tree)]
                [plot-p-node-with-children
                 (lambda (p-node c-nodes)
                   (cond
                     [(null? c-nodes) `()]
                     [else
                      (cons
                        (list (get-key p-node) (get-key (car c-nodes)))
                        (plot-p-node-with-children p-node (cdr c-nodes)))]))])
         (append (plot-p-node-with-children tree child-trees)
                 (plot-trees child-trees)))])))
               
;; plot-bst can plot binary search tree
(define plot-bst
  (lambda (tree)
    (cond 
      [(null? tree) `()]
      [else
       (append
           (list 
             (list (get-key tree) (get-key (left-child tree)))
             (list (get-key tree) (get-key (right-child tree))))
           (plot-bst (left-child tree)) 
           (plot-bst (right-child tree)))])))

(define remove-char-in-string
  (lambda (str ch)
    (letrec*
      ([lst-str (string->list str)]
       [remove-char
       	(lambda (lst ch)
         	(cond
           	[(null? lst) `()]
           	[(if (eq? (car lst) ch)
                	(remove-char (cdr lst) ch)
                	(cons (car lst) (remove-char (cdr lst) ch)))]))])
      (list->string (remove-char lst-str ch)))))

; convert a list of (x y) to graphviz's dot language
; (x y) means y connected to x
(define convert-to-graphviz
  (lambda (lst)
    (letrec
      ([fun-convert
        (lambda (lst)
          (let* ([s-node (car lst)]
                [e-node (cond
                          [(null? (cadr lst)) (format "{~a[label=\"NULL\" shape=box]}"
                                                (remove-char-in-string (gensym->unique-string (gensym (format "~a" s-node))) #\-))]
                          [else (cadr lst)])])
            (format "~a--~a;" s-node e-node)))]
       [rec-convert
        (lambda (lst)
          (cond
            [(null? lst) `()]
            [else
             (cons (fun-convert (car lst)) (rec-convert (cdr lst)))]))])
      (rec-convert lst))))

(define plot-tree-in-gv
  (lambda (tree)
    (apply string-append
           (append (list "graph G{") 
                   (convert-to-graphviz (plot-bst tree))
                   (list "}")))))

;; write binary search tree in graphviz dot language to a file
(define write-tree-to-file
  (lambda (tree file-name)
    (call-with-port (open-file-output-port file-name
                      (file-options no-fail)
                      (buffer-mode block)
                      (native-transcoder))
      (lambda (p)
        (put-string p (plot-tree-in-gv tree))))))

(define cons-dot-command
  (lambda (pkg dot-file-name)
    (format "dot -T~a ~a -o ~a" pkg dot-file-name (string-append dot-file-name "." pkg))))

;(display (get-first-ele-inorder-traversal tree))
;(newline)
(write-tree-to-file tree "hello.gv")
(system (cons-dot-command "png" "hello.gv"))
(write-tree-to-file (delete tree 65) "hello1.gv")
(system (cons-dot-command "png" "hello1.gv"))