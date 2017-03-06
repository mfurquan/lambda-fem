;(load "utils")

(define (linspace a b N)
  (let ((h (/ (- b a) (- N 1))))
    (map (lambda (i) (+ a (* h i)))
	 (enumerate 0 N))))

(define (outer f u v)
  (map (lambda (a)
	 (map (lambda (b)
		(f a b))
	      v))
       u))

(define (scale k v)
  (map (lambda (x) (* k x)) v))

(define (transpose A)
  (define (iter mat tran)
    (if (null? (car mat))
      (reverse tran)
      (iter (map cdr mat) (cons (map car mat) tran))))
  (iter A '()))

(define (augment A b) (map cons b A))
