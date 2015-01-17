;; The below program is ported from java code in the course Algorithms, Part1 in Coursera
;;
;; find item on left to swap
(define find-i-in-left-subVec
  (lambda (vec lo hi i)
    (cond 
      [(= hi i) i]
      [(>= (vector-ref vec i) (vector-ref vec lo)) i]
      [else (find-i-in-left-subVec vec lo hi (add1 i))])))

;; find item on right to swap
(define find-j-in-right-subVec
  (lambda (vec lo hi j)
    (cond
      [(= lo j) j]
      [(<= (vector-ref vec j) (vector-ref vec lo)) j]
      [else (find-j-in-right-subVec vec lo hi (sub1 j))])))

;; swap two elements in the vector
(define swap
  (lambda (vec i j)
    (let ([temp (vector-ref vec j)])
      (begin
        (vector-set! vec j (vector-ref vec i))
        (vector-set! vec i temp)))))

(define quicksort
  (lambda (vec lo hi)
    (letrec ([partial-quicksort
              ; initially lo is index 0, hi is last element's index (vector-length vec)-1, i is 1, j is hi
              (lambda (vec lo hi i j)
                (let ([i (find-i-in-left-subVec vec lo hi i)]
                      [j (find-j-in-right-subVec vec lo hi j)])
                	(cond
                   [(>= i j) j]
                   [else
                    (begin
                      (swap vec i j)
                      (partial-quicksort vec lo hi i j))])))])
             (cond
               [(>= lo hi) vec]
               [else (let ([j (partial-quicksort vec lo hi (add1 lo) hi)]) 
                       (begin
                         (swap vec lo j)
                         (quicksort vec lo (sub1 j))
                         (quicksort vec (add1 j) hi)))]))))

(define vec `#(2 3 6 8 6 4 10 1))
(display (quicksort vec 0 (sub1 (vector-length vec))))

