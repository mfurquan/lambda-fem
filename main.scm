(load "mesh")
(load "integrate")

(define degree 1)
(define no-elem 5)

(define mesh (mesh-segment 0 1 degree no-elem))

(define no-node (vector-length (node mesh)))

(define no-dof (- no-node 1))

(define intg-rule (gauss-legendre (ceiling (/ (+ degree 1) 2))))
