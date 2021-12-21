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

(define (fuel-required-part2 alignment crabs)
  (for/sum ([pos crabs])
    (letrec ([difference (abs (- alignment pos))]
             [step-cost (apply + (range 1 (+ 1 difference)))])
      step-cost)))

(define (check-alignments fuel-check alignments crabs)
  (sort
   (for/list ([alignment alignments])
     (list (fuel-check alignment crabs) alignment))
   (lambda (a b) (< (car a) (car b)))))

(define (try-solve fuel-check crabs)
  (let ([range (in-range (apply min crabs) (apply max crabs))])
    (check-alignments fuel-check range crabs)))
