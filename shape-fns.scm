(load "interpolation")

(define N (map (lambda (i)
		 (list (lambda (x) (lagr    i x))
		       (lambda (x) (lagr_xi i x))))
	       (enumerate 0 (+ degree 1))))

(define (deriv phi xi)
  (if (list? (car phi))
    (map (lambda (psi) (deriv psi xi)) phi)
    (cond ((= xi 0) (car  phi))
	  ((= xi 1) (cadr phi)))))
