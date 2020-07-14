;; For Outer walls only 
(grid (x y i)
      :shift ((rect-l column) 0)
      :size ((1- m) 2)
      :at (l (+ (reduce #'+ b) (rect-b column) (- (rect-b wall_x))))
      (add s wall_x :x :y y :base :bl :l (- (aref l i) (rect-l column))))

(grid (x y nil j)
      :shift (0 (rect-l column))
      :size (2 (1- n))
      :at ((+ (reduce #'+ l) (rect-l column) (- (rect-l wall_x))) b)
      (add s wall_y :x :y y :base :bl :b (- (aref b i) (rect-b column))))

;;
;; ;; change to global matrix
;; (grid (x y)
;; 	  :shift (0 0)
;; 	  :size (m n)
;; 	  :at (l b) :do
;; 	  ;; for column at (x, y) create the transformation matrices
;; 	  ;; Ax and Ay that change
;; 	  ;; x,y,theta of each floor from global coordinates
;; 	  ;; (x1,y1,theta1, x2,y2, theta2, ...) to
;; 	  ;; local coordinates (x1, x2,... )  by Ax
;; 	  ;; and (y1, y2, ...) by Ay
;; 	  (let ((Ax (make-array (list h (* 3 h))))
;; 			(Ay (make-array (list h (* 3 h)))))

;; 		))
