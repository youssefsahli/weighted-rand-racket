#lang racket

(provide weighted-rand)

(require "binary-search.rkt")

(define (weighted-rand choices-hash)
  (define (get-choice-weight key)
    (hash-ref choices-hash key 1))

  (let ([choice-list (hash->list choices-hash)])
    (let ([keys (hash-keys choices-hash)])
      (define (hash-cumulative-weight _hash)
        (reverse (foldr
                  (λ (key acc)
                    (cond
                      [(list? acc)
                       (cons `(,key . ,(+ (get-choice-weight key) (cdr (first acc))))
                             acc)]
                      [else `((,key . ,(get-choice-weight key)))]))
                  0
                  keys)))

      (define (pairs->cdr-list lop)
        (map (λ (p)
               (cdr p))
             lop))

      (let* ([cumulative-weights-pairs (hash-cumulative-weight choices-hash)]
             [cumulative-weights (pairs->cdr-list cumulative-weights-pairs)]
             [weight-sum (last cumulative-weights)]
             [R (random weight-sum)]
             [id (binary-search cumulative-weights R)]
             [r (car (list-ref cumulative-weights-pairs id))])
        (begin r)))))

(module+ test
  (require rackunit)

  (let ([distribution (hash 'A 20 'B 30 'C 50)]
        [counter (make-hash '((A . 0) (B . 0) (C . 0)))]
        [times 10000]
        [results (make-hash)])
    
    (for ([times (in-range times)])
      (let ([R (weighted-rand distribution)])
        (hash-update! counter R add1)))
    (hash-for-each counter
     (λ (k v) (hash-set! results k (exact->inexact (/ v times)))))
    results))
       