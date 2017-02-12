(define v (make-vector 3 0))

(define (morph a)
  (define (iter v c d)
    (if (= c 3) v
      (iter v (+ c 1) (vector-set! v c c))))
  (iter a 0 'b))
