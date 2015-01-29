;https://oj.leetcode.com/problems/letter-combinations-of-a-phone-number/

(define digit->letters
  (lambda (n)
    (let ([letters `("abc" "def" "ghi" "jkl" "mno" "pqrs" "tuv" "wxyz")])
      (cond
        [(or (= n 1) (> n 9)) #f]
        [else (list-ref letters (- n 2))]))))

(define partial-number->list
  (lambda (num)
    (cond
      [(zero? num) `()]
   		[else (cons (mod num 10) (partial-number->list (div num 10)))])))

; convert a numebr to list (number->list 234) => (2 3 4)
(define number->list
  (lambda (num)
    (reverse (partial-number->list num))))

(define combine
  (lambda (ele lst)
    (cond
      [(null? lst) `()]
      [else (cons (string-append ele (car lst)) (combine ele (cdr lst)))])))

; (combine-two-list `(a b c) `(d f g)) => (ad af ag bd bf bg ...)
(define combine-two-list
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) `()]
      [else (append (combine (car lst1) lst2) (combine-two-list (cdr lst1) lst2))])))

(define letter-combinations
  (lambda (str)
    (letrec ([lst-of-lst (map string->list (map digit->letters (number->list (string->number str))))]
             ;convert a list of list of char to list of list of string
             [char->string
              (lambda (lst-of-lst)
                (cond
                  [(null? lst-of-lst) `()]
                  [else (cons (map string (car lst-of-lst)) (char->string (cdr lst-of-lst)))]))]
          	[partial-letter-combinations
             (lambda (lst-of-lst)
             (cond
               [(null? (cdr lst-of-lst)) (car lst-of-lst)]
               [else (partial-letter-combinations (cons (combine-two-list (car lst-of-lst) (cadr lst-of-lst)) (cddr lst-of-lst)))]))])
      (partial-letter-combinations (char->string lst-of-lst)))))


(display (letter-combinations "334"))
