(define (identity x) x)

(define (inverse x) (/ 1 x))

(define (o f g)
  (lambda (x) (f (g x))))
