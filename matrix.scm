;#lang r5rs
(define (matrix:make rows cols . val)
  (define value (if (null? val) 0 (car val)))
  (list rows
	cols
	(do ((vec (make-vector rows))
	     (count 0 (+ count 1)))
	  ((= count rows) vec)
	  (vector-set! vec count (make-vector cols value)))))

(define (matrix:no-rows matrix) (car matrix))
(define (matrix:no-cols matrix) (cadr matrix))

(define (matrix:ref matrix row . col)
  (if (null? col) (vector-ref (list-ref matrix 2) row)
    (vector-ref (vector-ref (list-ref matrix 2) row) (car col))))

(define (matrix:set! matrix val row . col)
  (if (null? col) (vector-set! (list-ref matrix 2) row val)
    (vector-set! (matrix:ref matrix row) (car col) val)))

(define (row-transform matrix using target factor)
  (let ((ncol (matrix:no-rows matrix)))
    (do ((icol 0 (+ icol 1)))
      ((= icol ncol))
      (matrix:set! matrix
		   (+ (matrix:ref matrix target icol)
		      (* (matrix:ref matrix using icol) factor))
		   target
		   icol))))

;--------------- Gauss' Elimination --------------------------------

(define (triangulate matrix)
  (let ((nrow (matrix:no-rows matrix))
	(ncol (matrix:no-cols matrix)))

    ; row transform from a specified column
    (define (partial-row-transform from-col using-row target-row factor)
      (do ((icol from-col (+ icol 1)))
	((= icol ncol))
	(matrix:set! matrix
		     (- (matrix:ref matrix target-row icol)
			(* (matrix:ref matrix using-row icol) factor))
		     target-row
		     icol)))

    (define (eliminate col using-row target-row)
      (partial-row-transform col
			     using-row
			     target-row
			     (/ (matrix:ref matrix target-row col)
				(matrix:ref matrix using-row  col))))

    (define (eliminate-col col)
      (do ((irow (+ col 1) (+ irow 1)))
	((= irow nrow))
	(eliminate col col irow)))

    (define (pivoting irow)
      (define (exchange i j)
	(let ((c 0))
	  (do ((icol i (+ icol 1)))
	    ((= icol ncol))
	    (begin (set! c (matrix:ref matrix i icol))
		   (matrix:set! matrix
				(matrix:ref matrix j icol)
				i
				icol)
		   (matrix:set! matrix c j icol)))))
      (let ((prow (pivot-row irow)))
	(if (not (= irow prow))
	  (exchange irow prow))))

    (define (pivot-row irow)
      (do ((crow irow (+ crow 1))
	   (prow irow)
	   (pval (matrix:ref matrix irow irow)))
	((= crow nrow) prow)
	(let ((cval (abs (matrix:ref matrix crow irow))))
	      (if (> cval pval)
		(begin (set! pval cval)
		       (set! prow crow))))))
    
    (do ((icol 0 (+ icol 1)))
      ((= icol (- ncol 1)))
      (begin (pivoting icol)
	     (eliminate-col icol)))))

(define (back-subs tri-mat)
  (let ((nrow (matrix:no-rows tri-mat))
	(ncol (matrix:no-cols tri-mat))
	(sol-vec (make-vector (matrix:no-rows tri-mat))))
    (define (known-sum row)
      (define (iter icol ans)
	(if (= icol nrow)
	  ans
	  (iter (+ icol 1)
		(+ ans (* (matrix:ref tri-mat row icol)
			  (vector-ref sol-vec icol))))))
      (iter (+ row 1) 0))

    (do ((irow (- nrow 1) (- irow 1)))
      ((< irow 0) sol-vec)
      (vector-set! sol-vec
		   irow
		   (/ (- (matrix:ref tri-mat irow (- ncol 1))
			 (known-sum irow))
		      (matrix:ref tri-mat irow irow))))))
