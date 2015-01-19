(define merge-two-list
  (lambda (lon1 lon2)
    (cond
      [(and (null? lon1) (null? lon2)) `()]
      [(null? lon1) lon2]
      [(null? lon2) lon1]
      [else 
       (cond
        [(> (car lon1) (car lon2)) (cons (car lon2) (merge-two-list lon1 (cdr lon2)))]
        [(< (car lon1) (car lon2)) (cons (car lon1) (merge-two-list (cdr lon1) lon2))]
        [(= (car lon1) (car lon2)) (cons (car lon1) (cons (car lon2) (merge-two-list (cdr lon1) (cdr lon2))))])]
    )))

(define partial-merge
  (lambda (lst)
    (cond 
      [(null? lst) `()]
      [(null? (cdr lst)) lst] ;only as one element, like (3)
      [ else (cons (merge-two-list (car lst) (cadr lst)) (partial-merge (cddr lst)))])))
;;For tha above program, think about 
;; (partial-merge (cons (merge-two-list (car lst) (cadr lst)) (partial-merge (cddr lst))))

(define repeat-merge
  (lambda (lst)
    (cond
      [(null? lst) `()]
      [(null? (cdr lst)) (car lst)]
      [else (repeat-merge (partial-merge lst))])))

(define merge-sort
  (lambda (lst)
    (repeat-merge (map list lst))))

;; construct a list of n random numbers
(define generate-random-numbers
  (lambda (n)
    (if (zero? n)
        `()
        (cons (random 100000) (generate-random-numbers (sub1 n))))))

; a tight version of merge-sort
(define new-merge-sort
  (lambda (lst)
    (letrec ([merge-pairs
              (lambda (list-of-lists)
                (cond
                  ((null? list-of-lists) null)
                  ((null? (cdr list-of-lists)) list-of-lists)
                  (else (cons (merge-two-list (car list-of-lists) (cadr list-of-lists))
                              (merge-pairs (cddr list-of-lists))))))]
             [repeat-merge
              (lambda (list-of-lists)
                (if (null? (cdr list-of-lists))
                    (car list-of-lists)
                    (repeat-merge (merge-pairs list-of-lists))))])
      (repeat-merge (map list lst)))))


