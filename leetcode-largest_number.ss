;Leetcode question Largest Number
;Given a list of non negative integers, arrange them such that they form the largest number.
;For example, given [3, 30, 34, 5, 9], the largest formed number is 9534330.
;Note: The result may be very large, so you need to return a string instead of an integer.

(define number-to-list
  (lambda (num)
    (letrec ([num-to-lst
              (lambda (num)
                (cond
                  [(zero? num) `()]
                  [else (cons (mod num 10) (num-to-lst (div num 10)))]))])
      (reverse (num-to-lst num)))))

;build a numebr through a list of digits
(define build-number
  (lambda (lst)
    (string->number (apply string-append (map number->string lst)))))

(define compare-two-num-list
  (lambda (lst1 lst2)
    (cond
      [(and (null? lst1) (null? lst2)) 0]
      [(> (car lst1) (car lst2)) 1]
      [(< (car lst1) (car lst2)) -1]
      [(= (car lst1) (car lst2)) (compare-two-num-list (cdr lst1) (cdr lst2))])))

(define compare-two-numebr
  (lambda (num1 num2)
    (let* ([num1-lst (number-to-list num1)]
           [num2-lst (number-to-list num2)]
           [new-num1-lst (append num1-lst num2-lst)]
           [new-num2-lst (append num2-lst num1-lst)])
      (compare-two-num-list new-num1-lst new-num2-lst))))

(define merge-two-list
  (lambda (lon1 lon2 compare)
    (cond
      [(and (null? lon1) (null? lon2)) `()]
      [(null? lon1) lon2]
      [(null? lon2) lon1]
      [else 
       (cond
        [(= 1  (compare (car lon1) (car lon2))) (cons (car lon2) (merge-two-list lon1 (cdr lon2) compare))]
        [(= -1 (compare (car lon1) (car lon2))) (cons (car lon1) (merge-two-list (cdr lon1) lon2 compare))]
        [(= 0  (compare (car lon1) (car lon2))) (cons (car lon1) (cons (car lon2) (merge-two-list (cdr lon1) (cdr lon2) compare)))])])))

(define merge-sort
  (lambda (lst compare)
    (letrec ([merge-pairs
              (lambda (list-of-pairs)
                (cond
                  ((null? list-of-pairs) `())
                  ((null? (cdr list-of-pairs)) list-of-pairs)
                  (else (cons (merge-two-list (car list-of-pairs) (cadr list-of-pairs) compare)
                              (merge-pairs (cddr list-of-pairs))))))]
             [repeat-merge
              (lambda (list-of-pairs)
                (if (null? (cdr list-of-pairs))
                    (car list-of-pairs)
                    (repeat-merge (merge-pairs list-of-pairs))))])
      (repeat-merge (map list lst)))))

;; not inplace insertion sort
(define insertion-sort
  (lambda (lst compare)
    (letrec
      ([sort
        (lambda (lst ele compare)
          (cond
            [(null? lst) (list ele)]
            [(= 1 (compare (car lst) ele)) (cons ele lst)]
            [else (cons (car lst) (sort (cdr lst) ele compare))]))]
       [repeat-insertion-sort
        (lambda (lst1 lst2 compare)
          (cond
            [(null? lst2) lst1]
            [else (repeat-insertion-sort (sort lst1 (car lst2) compare) (cdr lst2) compare)]))])
      (repeat-insertion-sort (list (car lst)) (cdr lst) compare))))

      
(display (apply string-append (map number->string (reverse (merge-sort `(3 30 34 5 9) compare-two-numebr)))))
;(display (compare-two-numebr 3 9))




