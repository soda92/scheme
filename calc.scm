
(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
        (else (error "Calc: bad expression." exp))))

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + args))
        ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
                           ((= (length args) 1) (- (car args)))
                           (else (- (car args) (accumulate + (cdr args))))))
        ((eq? fn '*) (accumulate * args))
        ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
                           ((= (length args) 1) (/ (car args)))
                           (else (/ (car args) (accumulate * (cdr args))))))
        (else (error "Calc: bad operator: " fn))))

