#lang racket/gui

(require 2htdp/image)
(require 2htdp/batch-io)

(define-struct Point (x y))

(define create-plot-point
  (circle 1 "solid" "red"))

(define (read-data-set filename)
  (read-csv-file/rows filename (lambda (x) x))
  )

(define (convert-list lst)
  (map (lambda 
           (l) 
         (make-Point 
          (string->number (first l)) 
          (string->number (last l))))
       lst))

(define td 
  (rest 
   (read-data-set "test.csv")))

(define (extract-x lst)
  (map (lambda
           (l)
         (string->number (first l))
         ) lst))

(define (extract-y lst)
  (map (lambda
           (l)
         (string->number (last l))
         ) lst))

(define final-data (convert-list td))

(define (sum lst)
      (cond
        [(empty? lst) 0]
        [else (+ (first lst) (sum (rest lst)))]))

(define (mean lst)
  (/ (sum lst) (length lst))
  )

(define (embed-circle P backimage)
  (place-image create-plot-point (Point-x P) (Point-y P) backimage)
  )

(define (embed-engine lst)
  (cond
    [(empty? lst) (empty-scene 500 500)]
    [else (embed-circle 
           (first lst)
           (embed-engine (rest lst)))]
   )
 )

;;-----------------------------------------------------------------;;

(define X-vec (extract-x td))
(define Y-vec (extract-y td))
(define N (length final-data))
(define X-mean (mean X-vec))
(define Y-mean (mean Y-vec))

(define covariance-xy (/ (sum (map (lambda
         (l)
       (*(-(Point-x l) (mean X-vec)) (-(Point-y l) (mean Y-vec)))
       ) final-data)) (- N 1)))

(define variance-x (/ (sum (map (lambda
         (l)
       (*(-(Point-x l) (mean X-vec)) (-(Point-x l) (mean X-vec)))
       ) final-data)) (- N 1)))

(define Beta (/ covariance-xy variance-x))
(define Alpha (- Y-mean (* Beta X-mean)))

(flip-vertical (embed-engine final-data))
