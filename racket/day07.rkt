#lang racket

(require racket/port)
(require racket/string)

(define day7-input
  (let ([path "../input/day7.txt"])
    (for/list
        ([crab (string-split (port->string (open-input-file path)) ",")])
      (string->number (string-trim crab)))))

(define (fuel-required alignment crabs)
  (for/sum ([pos crabs]) (abs (- alignment pos))))

(define (check-alignments alignments crabs)
  (sort
   (for/list ([alignment alignments])
     (list (fuel-required alignment crabs) alignment))
   (lambda (a b) (< (car a) (car b)))))

(define (try-solve crabs)
  (let ([range (in-range (apply min crabs) (apply max crabs))])
    (check-alignments range crabs)))
