(load "lblas")
(load "shape-fns")
(load "inner-prod")

(define (f x) 1)

(define (f_k-ele elem-nods)
  (define xi-map (interpolate-f N elem-nods))
  (augment
    (outer (lambda (p q)
	     (inner-11 p
		       q
		       xi-map
		       intg-rule))
	   N N)
    (map (lambda (p)
	   (inner-00 p
		     (interpolate-f N (map f elem-nods))
		     xi-map
		     intg-rule))
	 N)))

(define (id-ele elem-con)
  (map (lambda (i) (id mesh i)) elem-con))

(define (x-ele elem-con)
  (map (lambda (i) (node mesh i)) elem-con))

(define (loc-mat elem-con)
  (let ((id-lst (id-ele elem-con)))
    (augment
      (outer (lambda (i j)
	       (if (> j -1) (list i (+ j 1)) (list i j)))
	       id-lst id-lst)
      (map (lambda (i) (list i 0)) id-lst))))
