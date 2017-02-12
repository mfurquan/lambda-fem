(define (make-matrix rows columns init_val)
  (define (iter count-rows matrix dummy)
    (if (< count-rows 0) 
      matrix
      (iter (- count-rows 1)
	    matrix
	    (vector-set! matrix
			 count-rows
			 (make-vector columns init_val)))))
  (iter (- rows 1) (make-vector rows) 'dummy))

(define (matrix-ref matrix row-index col-index)
  (vector-ref (vector-ref matrix row-index) col-index))

(define (matrix-set! matrix row-index col-index value)
  (vector-set! (vector-ref matrix row-index) col-index value))

(define (no-rows matrix)
  (vector-length matrix))

(define (no-cols matrix)
  (vector-length (vector-ref matrix 1)))

(define (row-ref matrix row-index)
  (vector-ref matrix row-index))

(define (row-set! matrix row-index row)
  (vector-set! matrix row-index row))

(define (vector-map proc vec)
  (define n (vector-length vec))
  (define (iter count result dummy)
    (if (< count 0)
      result
      (iter (- count 1)
	    result
	    (vector-set! result
			 count
			 (proc (vector-ref result count))))))
  (iter (- n 1) vec 'dummy))
