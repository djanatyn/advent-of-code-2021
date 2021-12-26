#lang racket

(require racket/port)
(require racket/dict)

(define day9-input
  (let ([path "../input/day9.txt"]) (port->string (open-input-file path))))

(define example-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(define (parse input)
  (for/vector ([line (string-split input "\n")])
    (list->vector (string->list line))))

(struct coord (x y)
  #:transparent)

(define (in-bounds? height width coord)
  (and
   (>= (coord-x coord) 0)
   (>= (coord-y coord) 0)
   (<= (coord-x coord) (- width 1))
   (<= (coord-y coord) (- height 1))))

(define (offset c1 c2)
  (coord
   (+ (coord-x c1) (coord-x c2))
   (+ (coord-y c1) (coord-y c2))))

(define (adjacent-coords origin height width)
  (letrec ([n (coord 0 1)]
           [s (coord 0 -1)]
           [w (coord -1 0)]
           [e (coord 1 0)])
           ;; [ne (coord 1 1)]
           ;; [nw (coord -1 1)]
           ;; [se (coord 1 -1)]
           ;; [sw (coord -1 -1)])
    (filter
     (lambda (point) (in-bounds? height width point))
     (for/list ([direction (list n s w e)])
        (offset origin direction)))))

(define (get coord heightmap)
  (vector-ref
   (vector-ref heightmap (coord-y coord))
   (coord-x coord)))

(define (adjacent coord heightmap)
  (letrec ([height (vector-length heightmap)]
           [width (vector-length (vector-ref heightmap 0))]
           [valid-neighbors (adjacent-coords coord height width)])
    (for/list ([coord valid-neighbors])
      (get coord heightmap))))

(define example-parsed (parse example-input))

(struct evaluation (value location neighbors)
  #:transparent)

(define (char->number char)
  (string->number (list->string (list char))))

(define (low-points heightmap width height)
  (for/list ([y (range 0 height)])
    (filter
     (lambda (cell)
       (for/and ([neighbor (evaluation-neighbors cell)])
         (letrec ([value (evaluation-value cell)]
                  [value-num (char->number value)]
                  [neighbor-num (char->number neighbor)])
           (< value-num neighbor-num))))
     (for/list ([x (range 0 width)])
       (letrec ([origin (get (coord x y) heightmap)]
                [neighbor-values (adjacent (coord x y) heightmap)])
         (evaluation origin (coord x y) neighbor-values))))))

(define solution-low-points
  (flatten
   (letrec ([input (parse day9-input)]
            [height (vector-length input)]
            [width (vector-length (vector-ref input 0))])
     (low-points input width height))))

(for/sum ([low-point solution-low-points])
  (+ 1 (char->number (evaluation-value low-point))))
