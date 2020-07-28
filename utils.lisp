(in-package #:fundamental-period)

(defmacro grid (&rest form-and-body)
  "Run code at grid points
Example:
;; (grid (x y)
;; 		  :origin (1 0)
;; 		  (2 :by 3)
;; 		  (:at l b) :do
;; 		  (print (list x y)))"
  (trivia:match form-and-body
    ((list* (list* x y ij)
	    :shift (list x0 y0)
	    :size (list m n)
	    :at (list l b)
	    :do body)
     (alexandria:once-only (m n l b y0)
       (alexandria:with-gensyms (i j)
	 (if (first ij)
	     (setf i (first ij)))
	 (if (second ij)
	     (setf j (second ij)))
	 `(loop with ,x = ,x0
		for ,i from 0 below ,m do 
		  (loop with ,y = ,y0
			for ,j from 0 below ,n do
			,@body
			  (unless (= ,j (- ,n 1))
			    (incf ,y (cond ((numberp ,b) ,b)
					   ((vectorp ,b) (aref ,b ,j))
					   (t (error "invalid ~a" ,b))))))
		  (unless (= ,i (- ,m 1))
		    (incf ,x (cond ((numberp ,l) ,l)
				   ((vectorp ,l) (aref ,l ,i))
				   (t (error "invalid ~a" ,l)))))))))
    ((list* xi
	    :shift x0
	    :size m
	    :at l
	    :do body)
     (alexandria:once-only (m l)
       (let (x i)
	 (cond ((listp xi)
		(setf x (first xi))
		(setf i (second xi)))
	       (t
		(setf x xi)
		(setf i (gensym "i"))))
	 `(loop with ,x = ,x0
		for ,i from 0 below ,m do 
		,@body
		  (unless (= ,i (- ,m 1))
		    (incf ,x (cond ((numberp ,l) ,l)
				   ((vectorp ,l) (aref ,l ,i))
				   (t (error "invalid ~a" ,l))
				   )))))))))

;;;; Shapes
(defstruct (rect
	    (:constructor make-rect))
  "Rectangular shape; density is in kg/m3; area-load in kg/m2"
  l b h mass density area-load)

(defun make-rect (&key l b h mass density area-load)
  (let ((r (make-instance 'rect)))
    (setf (slot-value r 'l) l)
    (setf (slot-value r 'b) b)
    (setf (slot-value r 'h) h)
    (setf (slot-value r 'density) density)
    (setf (slot-value r 'mass) (cond (mass mass)
				     ((and l b h density) (+ (* l b h density)
							     (* l b (or area-load 0d0))))
				     (t nil)))
    (setf (slot-value r 'area-load) (or area-load 0d0))
    r))


(defun offsety-of (rect1 &key from to of)
  "Offset from `from' point of `rect' to the `to' point of `of'"
  (flet ((y (rect point)
	   (ecase point
	     (:bottom 0)
	     (:centre (/ (rect-b rect) 2))
	     (:top (rect-b rect)))))
    (- (y of to) (y rect1 from))))

(defun offsetx-of (rect1 &key from to of)
  "Offset from `from' point of `rect' to the `to' point of `of'"
  (flet ((x (rect point)
	   (ecase point
	     (:left 0)
	     (:centre (/ (rect-l rect) 2))
	     (:right (rect-l rect)))))
    (- (x of to) (x rect1 from))))

;;; Matrix Manipulations


(defun zeros (dimensions)
  (make-array dimensions :initial-element 0))
(defun mat+ (mat1 mat2)
  (let* ((dim (array-dimensions mat1))
	 (res (make-array dim :initial-element 0)))
    (loop for i from 0 below (first dim) do
      (loop for j from 0 below (second dim) do
	(setf (aref res i j) (+ (aref mat1 i j)
				(aref mat2 i j)))))
    res))

(defun mat* (scalar mat)
  (let* ((dim (array-dimensions mat))
	 (res (make-array dim)))
    (loop for i from 0 below (first dim) do
      (loop for j from 0 below (second dim) do
	(setf (aref res i j) (* scalar (aref mat i j)))))
    res))

(defun transpose(mat1)
  (let* ((dim (array-dimensions mat1))
	 (res (make-array (reverse dim))))
    (loop for i from 0 below (second dim) do
      (loop for j from 0 below (first dim) do
	(setf (aref res i j) (aref mat1 j i))))
    res))

(defun matmul (mat1 mat2)
  (let* ((dim1 (array-dimensions mat1))
	 (dim2 (array-dimensions mat2))
	 (res (make-array (list (first dim1) (second dim2)))))
    (loop for i from 0 below (first dim1) do
      (loop for j from 0 below (second dim2) do
	(setf (aref res i j)
	      (loop for k from 0 below (second dim1)
		    summing (* (aref mat1 i k)
			       (aref mat2 k j))))))
    res))

(defmacro mincf(mat incm)
  `(setf ,mat (mat+ ,mat ,incm)))

(defun symmetric-p (mat)
  (let ((dim (array-dimensions mat)))
    (assert (= (first dim) (second dim)))
    (loop for i from 0 below (first dim) do
      (loop for j from 0 below i do
	(unless (= (aref mat i j) (aref mat j i))
	  (format t "At ~a,~a = ~a but at ~a,~a = ~a"
		  i j (aref mat i j)
		  j i (aref mat j i))
	  (return-from symmetric-p nil))))
    t))
