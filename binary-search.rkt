#lang racket

(provide binary-search)

(define (binary-search lst v [mid 0] [sup-or-eq? >=])
  (cond
    [(and (list? lst) (= (length lst) 1)) (if (> v (first lst)) (add1 mid) mid)]
    [(and (list? lst) (>= (length lst) 2))
     (let* ([_mid (floor (/ (length lst) 2))]
            [value (list-ref lst _mid)])
       (cond
         [(sup-or-eq? value v) (let ([left (take lst _mid)])
                      (binary-search left v mid sup-or-eq?))]
         
         [else (let* ([mid (or mid 0)]
                      [right (drop lst _mid)])
                     (binary-search right v (+ _mid mid) sup-or-eq?))]))]
    [else lst]))

(module+ test
  (require rackunit)
  (let* ([L (for/list ([i (in-range 10 1000 10)]) i)])
    (check-eq? (binary-search L 40) 3)
    (check-eq? (binary-search L 0) 0)
    (check-eq? (binary-search L 55) 5)))
  