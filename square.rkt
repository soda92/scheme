#lang sicp

(inc 1)
(define (inc2 x) (inc (inc x)))

(inc2 2)
