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

(define all-segments (list #\a #\b #\c #\d #\e #\f #\g))

(define (check-length n)
  (lambda (pattern) (= (string-length pattern) n)))

(define (solve entry)
  (letrec ([patterns (entry-unique-patterns entry)]
           [segments1 (findf (check-length 2) patterns)]
           [segments7 (findf (check-length 3) patterns)]
           [a (set-subtract
               (string->list segments7)
               (string->list segments1))]
           [segments4 (findf (check-length 4) patterns)]
           [segments9?
            (lambda (segments)
              (= 1 (length
                    (set-symmetric-difference
                     (append a (string->list segments4))
                     (string->list segments)))))]
           [segments9 (findf segments9? (filter (check-length 6) patterns))]
           [e (set-subtract
               all-segments
               (append a (string->list segments9)))]
           [segments8 (findf (check-length 7) patterns)]
           [g (set-symmetric-difference
                (string->list segments8)
                (append e a (string->list segments4)))]
           [length-6-segments (filter (check-length 6) patterns)]
           [segments0? (lambda (pattern)
                         (= 1 (length (set-symmetric-difference
                                       (string->list pattern)
                                       (append a e g (string->list segments1))))))]
           [segments0 (findf segments0? length-6-segments)]
           [b (set-symmetric-difference
               (string->list segments0)
               (append a e g (string->list segments1)))]
           [segments6? (lambda (pattern)
                         (= 1 (length (set-subtract
                                       (string->list segments1)
                                       (string->list pattern)))))]
           [segments6 (findf segments6? length-6-segments)]
           [c (set-subtract
               (string->list segments1)
               (string->list segments6))]
           [d (set-subtract
               (string->list segments4)
               (append b (string->list segments1)))]
           [f (set-subtract
               all-segments
               (map first (list a b c d e g)))])
    `((outputs . ,(entry-output-pattern entry))
      (patterns ,patterns)
      (mappings
       ((,(first a) #\a)
        (,(first b) #\b)
        (,(first c) #\c)
        (,(first d) #\d)
        (,(first e) #\e)
        (,(first f) #\f)
        (,(first g) #\g))))))

(define example-patterns
  (solve (parse-line example-input)))

(define solved
  (for/list ([entry parsed-input]) (solve entry)))

(define (map-char mappings char)
  (first (dict-ref mappings char)))

(define (interpret solution)
 (for/list ([entry solution])
   (for/list ([output (dict-ref entry 'outputs)])
     (list->string
      (for/list ([char (string->list output)])
        (map-char (first (dict-ref entry 'mappings)) char))))))

(define (translate word)
  (match (list->string (sort (string->list word) char<?))
    ["abcefg" 0]
    ["cf" 1]
    ["acdeg" 2]
    ["acdfg" 3]
    ["bcdf" 4]
    ["abdfg" 5]
    ["abdefg" 6]
    ["acf" 7]
    ["abcdefg" 8]
    ["abcdfg" 9]
    [_ (error "failed: ~a" word)]))

(for/sum ([entry (interpret solved)])
  (string->number
   (for/fold
       ([str ""])
       ([digit (map number->string (for/list ([word entry]) (translate word)))])
       (string-append str digit))))

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
