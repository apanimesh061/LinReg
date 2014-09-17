#lang racket
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(define newline "\n")

(define scaling-x 20)
(define scaling-y 4)

(define plot-frame-width 500)
(define plot-frame-height 500)

(define (scale-var var factor)
  (* var factor)
  )

(define-struct Point (x y) #:transparent)

(define create-plot-point
  (circle 2 "solid" "blue"))

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
  (place-image create-plot-point (* (Point-x P) scaling-x) (* (Point-y P) scaling-y) backimage)
  )

(define (embed-engine lst)
  (cond
    [(empty? lst) (empty-scene plot-frame-height plot-frame-width)]
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

(define (f-of-x n)
  (+ Alpha (* Beta n)))

(define Beta (/ covariance-xy variance-x))
(define Alpha (- Y-mean (* Beta X-mean)))
(define line-start 
  (make-Point 
   (* scaling-x 0) 
   (* scaling-y 
    (f-of-x 0))
   ))

(define line-end 
  (make-Point 
   (* scaling-x 25) 
   (* scaling-y 
    (f-of-x 25))
   ))

(define render-graph (flip-vertical
(add-line 
 (embed-engine final-data) 
 (Point-x line-start) 
 (Point-y line-start) 
 (Point-x line-end) 
 (Point-y line-end) 
 "red")))

(define equation (string-append 
 "y = " 
 (number->string Alpha) 
 " + " 
 (number->string Beta) 
 "x"))

render-graph
(display newline)
(string-append "Sample Size: " (number->string N))
(string-append "Mean x: " (number->string X-mean))
(string-append "Mean y: " (number->string Y-mean))
(string-append "Intercept (Alpha): " (number->string Alpha))
(string-append "Slope (Beta): " (number->string Beta))
(string-append "Regression line equation: " equation)
