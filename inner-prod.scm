;(load "integrate")

(define (inner-11 f g xi2x intg-rule)
  (integrate (lambda (xi)
	       (* (/ ((deriv f 1) xi) ((deriv xi2x 1) xi))
		  (/ ((deriv g 1) xi) ((deriv xi2x 1) xi))
		  ((deriv xi2x 1) xi)))
	     intg-rule))

(define (inner-00 f g xi2x intg-rule)
  (integrate (lambda (xi)
	       (* ((deriv f 0) xi) ((deriv g 0) xi)
		  ((deriv xi2x 1) xi)))
	     intg-rule))
