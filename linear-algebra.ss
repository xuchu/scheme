(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; need compute the square root after the call
(define vector-magnitude
  (lambda (lst)
    (apply + (map (lambda (x) (* x x)) lst))))

;; make vector by two vertexes
(define make-vec
  (lambda (vec1 vec2)
    (cond
      [(and (null? vec1) (null? vec2)) `()]
      [else (cons (- (car vec2) (car vec1)) (make-vec (cdr vec1) (cdr vec2)))])))

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

(define multiply-num-on-vector
  (lambda (num vec)
    (cond
      [(null? vec) `()]
      [else (cons (* num (car vec)) (multiply-num-on-vector num (cdr vec)))])))

(define add-two-vectors
  (lambda (vec1 vec2)
    (cond
      [(and (null? vec1) (null? vec2)) `()]
      [(null? vec1) vec2]
      [(null? vec2) vec1]
      [else (cons (+ (car vec1) (car vec2)) (add-two-vectors (cdr vec1) (cdr vec2)))])))

; (string-join `("a" "b") " ") => "a b "
(define string-join
  (lambda (lst-of-str str)
    (apply string-append (map (lambda (s) (string-append s str)) lst-of-str))))


(define multiply-two-vector
  (lambda (vec1 vec2)
    (let ([length (vector-length vec1)])
      (do ([result 0]
           [i 0 (add1 i)])
        ((= i length) result)
        (set! result (+ result (* (vector-ref vec1 i) (vector-ref vec2 i))))))))

; the maxtrix must be reversed
(define multiply-vec-with-matrix
  (lambda (v matrix)
    (if (matrix 'is-reversed?)
        (do ([vec (make-vector (matrix 'rows))]
             [i 0 (add1 i)])
          ((= i (matrix 'rows)) vec)
          (vector-set! vec i (multiply-two-vector v (matrix 'get-ith-rows i))))
        (raise 'matrix-not-reversed))))

(define multiply-two-matrix
  (lambda (matrixA matrixB)
    (matrixB 'reverse)
    (if (= (matrixA 'columns) (matrixB 'columns))
        (let ([rows (matrixA 'rows)])
          (do ([vec (make-vector rows)]
               [i 0 (add1 i)])
            ((= i rows) vec)
            (vector-set! vec i (multiply-vec-with-matrix (matrixA 'get-ith-rows i) matrixB))))
        (raise 'matrixA-columns-not-equal-matrixB-rows))))


(define matrix
  (lambda (lst-of-lst)
   (let* ([m (list->vector (map list->vector lst-of-lst))]
          [rows (vector-length m)]
          [columns (vector-length (vector-ref m 0))]
          [reversed? #f])
     
     (define is-reversed?
       (lambda ()
         reversed?))
     
     (define get-ith-rows
        (lambda (i)
          (if (< i rows)
            (vector-ref m i)
            (raise 'row-index-exceeds))))
     
     (define get-rows
       (lambda ()
         rows))
     (define reset-rows
       (lambda ()
         (set! rows (vector-length m))))
     
     (define get-columns
       (lambda ()
         columns))
     (define reset-columns
       (lambda ()
         (set! columns (vector-length (vector-ref m 0)))))
     
     (define convert->vec-of-vec
       (lambda (lst-of-lst)
         (list->vector (map list->vector lst-of-lst))))
     
     (define convert->lst-of-lst
       (lambda (vec-of-vec)
         (map vector->list (vector->list vec-of-vec))))
     
     (define elem
       (lambda (i j)
         (vector-ref (vector-ref m i) j)))
     
     (define show
       (lambda () (display (string-join (map (lambda (s) (string-join (map number->string s) " ")) (convert->lst-of-lst m)) "\n"))))
     
     (define reverse
       (lambda ()
         (let ([get-rev-vec
                (lambda (j)
                  (do ([vec (make-vector rows)]
                       [i 0 (add1 i)])
                    ((= i rows) vec)
                    (vector-set! vec i (elem i j))))])
           (do ([vec (make-vector columns)]
                [j 0 (+ j 1)])
             ((= j columns) (begin 
                              (set! m vec) 
                              (set! reversed? #t)
                              (reset-rows)
                              (reset-columns)
                              dispatch))
             (vector-set! vec j (get-rev-vec j))))))
     
     (define dispatch
       (lambda (message . args) 
         (apply (case message
                  ((is-reversed?) is-reversed?)
                  ((elem) elem)
                  ((show) show)
                  ((rows) get-rows)
                  ((get-ith-rows) get-ith-rows)
                  ((columns) get-columns)
                  ((reverse) reverse))
              args)))
   dispatch)))

(define m1 (matrix `((1 2) (3 4))))
(define m2 (matrix `((2 4 6) (3 5 7))))
(m2 'show)
(display (multiply-two-matrix m1 m2))
