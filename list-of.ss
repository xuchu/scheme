; two implementation of list-of

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (cond
        [(null? val) #t]
        [else (and (pair? val) (pred (car val)) ((list-of pred) (cdr val)))]))))