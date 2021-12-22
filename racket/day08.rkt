#lang racket

(require racket/port)

(define day8-input
  (let ([path "../input/day8.txt"]) (port->string (open-input-file path))))

(define example-input
  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

;; number of segments per digit:
;; - 0: 6
;; - 1: 2
;; - 2: 5
;; - 3: 5
;; - 4: 4
;; - 5: 5
;; - 6: 6
;; - 7: 3
;; - 8: 7
;; - 9: 6
;;
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
;; questions:
;; - if we know *segment mappings* from unique numbers, can we disambiguate the non-unique?
