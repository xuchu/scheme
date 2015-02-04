; need compute the square root after the call
(define vector-magnitude
  (lambda (lst)
    (apply + (map (lambda (x) (* x x)) lst))))

(define make-vector
  (lambda (vec1 vec2)
    (cond
      [(and (null? vec1) (null? vec2)) `()]
      [else (cons (- (car vec2) (car vec1)) (make-vector (cdr vec1) (cdr vec2)))])))

(define 2-dime-cross-product
  (lambda (lst1 lst2)
    (- (* (car lst1) (cadr lst2)) (* (cadr lst1) (car lst2)))))

(define second
  (lambda (lst)
    (cadr lst)))

(define third
  (lambda (lst)
    (caddr lst)))

(define dot-product
  (lambda (lst1 lst2)
    (cond
      [(and (null? lst1) (null? lst2)) 0]
    	[else (+ (* (car lst1) (car lst2)) (dot-product (cdr lst1) (cdr lst2)))])))

; 3 dimentional cross product
(define cross-product
  (lambda (lst1 lst2)
    (list
      (2-dime-cross-product (cdr lst1) (cdr lst2))
      (* -1 (2-dime-cross-product (list (car lst1) (third lst1))
                                  (list (car lst2) (third lst2))))
      (2-dime-cross-product (list (car lst1) (second lst1))
                            (list (car lst2) (second lst2))))))