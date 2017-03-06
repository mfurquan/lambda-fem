(load "utils")

(define degree 1)
(define no-elem 5)
(define g 1)
(define h 1)

(load "mesh")
(load "integrate")
(load "shape-fns")
(load "inner-prod")
(load "lblas")
(load "vblas")
(load "interpolation")
(load "local")

(define mesh (mesh-segment 0 1 degree no-elem))

(define no-node (vector-length (node mesh)))

(define no-dof (- no-node 1))

(load "global")

(define intg-rule (gauss-legendre (ceiling (/ (+ degree 1) 2))))

(assemble-all)
