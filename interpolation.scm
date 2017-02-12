(load "lblas")

(define degree 1)

;--------------------------------------------------------------

(define ref-nods (linspace -1 1 (+ degree 1)))

(define (lagr index xi)
  (let ((xia (list-ref ref-nods index)))
    (foldl * 1 (filt-map index
      		   (lambda (x)
      		     (/ (- xi  x)
      			(- xia x)))
      		   ref-nods))))

(define (lagr_xi index xi)
  (* (lagr index xi)
     (foldl + 0 (filt-map index
		    (lambda (x)
		      (/ 1 (- xi x)))
		    ref-nods))))

(define (interpolate f-list nodal-vals xi)
  (foldl + 0 (map (lambda (f x)
		    (* (f xi) x))
		  f-list
		  nodal-vals)))

(define (interpolate-f xi-map elem-nods)
  (list (lambda (xi)
	  (interpolate (deriv xi-map 0) elem-nods xi))
	(lambda (xi)
	  (interpolate (deriv xi-map 1) elem-nods xi))))
