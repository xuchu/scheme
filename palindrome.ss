
;; check is a number is a Palindrome Number

; (digits-in-num 456) => 3
(define digits-in-num
    (lambda (num)
      (cond
        [(zero? num) 0]
        [else (add1 (digits-in-num (div num 10)))])))

;from right to left, get i-th digit in the num
; (get-digit-in-num 2345 4) => 2
(define get-digit-in-num
  (lambda (num i)
    (cond
      [(= i 1) (mod num 10)]
      [else (get-digit-in-num (div num 10) (sub1 i))])))

(define palindrome?
  (lambda (num)
    (letrec ([partial-palindrome
              (lambda (num i j)
                (cond
                  [(or (= i j) (> i j)) #t]
                  [else (and (= (get-digit-in-num num i) (get-digit-in-num num j)) 
                             (partial-palindrome num (add1 i) (sub1 j)))]))])
    	(partial-palindrome num 1 (digits-in-num num)))))

(display (palindrome? 234432))
