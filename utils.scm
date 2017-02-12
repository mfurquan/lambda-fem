(define (enumerate initial number)
  (define (iter enum count)
    (if (< count initial) enum
      (iter (cons count enum) (- count 1))))
  (iter '() (+ initial number -1)))

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
      (iter (op result (car rest))
		(cdr rest))))
  (iter initial sequence))

(define (foldr op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
	(foldr op initial (cdr sequence)))))

(define (filter predicate sequence)
  (define (iter seq result)
    (cond ((null? seq) (reverse result))
	  ((predicate (car seq)) (iter (cdr seq)
				       (cons (car seq) result)))
	  (else (iter (cdr seq)
		      result))))
  (iter sequence '()))

(define (filt-map n proc . seq-list)
  (define (iter count seq result)
    (cond ((null? (car seq)) (reverse result))
	  ((= count 0) (iter (- count 1)
			     (map cdr seq)
			     result))
	  (else (iter (- count 1) (map cdr seq) (cons (apply proc (map car seq)) result)))))
  (iter n seq-list '()))
