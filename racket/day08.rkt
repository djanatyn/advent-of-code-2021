#lang racket

(require racket/port)

(define day8-input
  (let ([path "../input/day8.txt"]) (port->string (open-input-file path))))

(struct entry (unique-patterns output-pattern))

(define (parse-line input)
  (letrec ([split (string-split input "|")]
           [unique-patterns (string-split (first split) " ")]
           [output-pattern (string-split (last split) " ")])
    (entry unique-patterns output-pattern)))

(define example-input
  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(define parsed-input
  (for/list ([line (string-split day8-input "\n")]) (parse-line line)))

(define solution
 (for/sum
     ([line (for/list ([entry parsed-input])
              (filter-map
               (lambda (pattern) (if (member (string-length pattern) '(2 3 4 7)) pattern #f))
               (entry-output-pattern entry)))])
   (length line)))

(define (check-num-segments num-segments)
  (cond [(= num-segments 2) '(1)]
        [(= num-segments 3) '(7)]
        [(= num-segments 7) '(8)]
        [(= num-segments 5) '(2 3 5)]
        [(= num-segments 6) '(0 6 9)]
        [else (error "invalid input")]))

(define initial-mapping
  '((a . #f)
    (b . #f)
    (c . #f)
    (d . #f)
    (e . #f)
    (g . #f)))

;; unique number of segments:
;; - digit 1 -> 2 segments
;;   - mappings known for {c,f}
;; - digit 4 -> 4 segments
;;   - mappings known for {b,c,d,f}
;; - digit 7 -> 3 segments
;;   - mappings known for {a,c,f}
;; - digit 8 -> 7 segments
;;   - mappings known for {a,b,c,d,e,f,g} (all mappings!)
;;
;; non-unique number of segments:
;; (5 segments)
;; - digit 2 -> 5 segments
;;   - uses {a,c,d,e,g}, doesn't use {b,f}
;; - digit 3 -> 5 segments
;;   - uses {a,c,d,f,g}, doesn't use {b,e}
;; - digit 5 -> 5 segments
;;   - uses {a,b,d,f,g}, doesn't use {c,e}
;;
;; (6 segments)
;; - digit 0 -> 6 segments
;;   - uses {a,b,c,e,f,g}, doesn't use {d}
;; - digit 6 -> 6 segments
;;   - uses {a,b,d,e,f,g}, doesn't use {c}
;; - digit 9 -> 6 segments
;;   - uses {a,b,c,d,f,g}, doesn't use {e}
;;
;; first, look for known digits in examples
;; we now know 1,4,7,8 digits
;;
;; if you know which digits are 1 + 7, you can figure out `a` (difference in segments)
;; if you know mapping for {a}, and you know which digit is 4, 9 should match 4 (with an extra segment for {a})
;; if you know what 9's segments (with {a} removed) is, the missing unknown segment is {e}
;; if you know {e,a}, take them out of the digit 8, you're left with {b,c,d,f,g}
;; given {b,c,d,f,g}, take difference of 4, now you know mapping for {g}
;; given mapping for {e,a,g}, missing mappings are {b,c,d,f}
;; hmm, what's next?
