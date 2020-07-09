;;;; MOI.lisp

(in-package #:MOI)

;;; Looping on Grid 
;;; Shapes, Offsets
;;; Matrix manipulations

;;; Data types
(defstruct (building-geometry
	    (:conc-name bg-))
  number-of-storey
  l  ;; centre to centre distance betn columns in x dir
  b     ;; centre to centre distance betn columns in y dir
  h ;; height of floor
  )

(defstruct (structural-geometry
	    (:conc-name sg-))
  "floar-loads = Live Load and other loads (kN/m^2)"
  (column-size 0.300d0 :type double-float) ;; column is assumed square
  (beam-height 0.500d0 :type double-float)
  (wall-thickness 0.230d0 :type double-float)
  (slab-thickness 0.150d0 :type double-float)
  (floor-loads 3.5d0 :type double-float)
  (column-elasticity 25d9 :type double-float)
  (wall-elasticity 2.65d9 :type double-float))

;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mass Matrix
;;;;
;;;;;;;;;;;;;;;;;;;;

;;; Moment of Inertia
(defstruct moi-sums
  "Sums used to calculate Moment of inertia"
  (mx 0) (my 0) (m 0) (ix 0) (iy 0))

(defstruct moi
  "Moment of inertia values"
  xc yc mass ixx iyy ipc)

(defmethod moi ((s moi-sums))
  (with-slots (mx my m ix iy) s
    (let ((moi (make-moi :xc  (coerce  (/ mx m) 'double-float)
			 :yc  (coerce  (/ my m) 'double-float)
			 :mass m
			 :ixx (coerce  (- ix (* m (expt (/ my m) 2))) 'double-float)
			 :iyy (coerce  (- iy (* m (expt (/ mx m) 2))) 'double-float))))
      (with-slots (ixx iyy xc yc m) moi
	(setf (slot-value moi 'ipc)
	      (+ ixx iyy)))
      moi)))

(defmethod add ((s moi-sums) (r rect) &key x y
					(base :bl)
					(l (slot-value r 'l))
					(b (slot-value r 'b))
					(mass (slot-value r 'mass)))
  "Add a shape to the sum. Attachment point is `base'"
  (with-slots (mx my m ix iy) s
    (let (xc yc)
      (unless mass
	(setf mass   (+ (* l b (slot-value r 'h) (slot-value r 'density))
			(* l b (or (slot-value r 'area-load) 0d0)))))
      (ecase base
	(:bl   ;; Botton Left
	 (setf xc (+ x (/ l 2))
	       yc (+ y (/ b 2))))
	(:centre
	 (setf xc x
	       yc y))
	(:bc   ;; Bottom Centre
	 (setf xc x
	       yc (+ y (/ b 2))))
	(:cl   ;; Centre Left
	 (setf xc (+ x (/ l 2))
	       yc y)))
      (reporting
       (format t "~& mass = ~,3f * ~,3f * ~,3f * ~,3f = ~,3f, " (slot-value r 'density) l b (slot-value r 'h) mass)
       (format t "at (~a, ~a)" xc yc))
      (incf mx (* mass xc))
      (incf my (* mass yc))
      (incf m  mass)
      (incf ix (+ (* mass 1/12 (expt b 2))
		  (* mass (expt yc 2))))
      (incf iy (+ (* mass 1/12 (expt l 2))
		  (* mass (expt xc 2))))
      (reporting :vverbose
		 (print "Moment of Inertias: ")
		 (print (moi (print (make-moi-sums :mx (* mass xc) :my (* mass yc) :m mass
						   :ix (+ (* mass 1/12 (expt b 2)) (* mass (expt yc 2)))
						   :iy  (+ (* mass 1/12 (expt l 2)) (* mass (expt xc 2))))))))
      
      (values mass xc yc))))

(defun mass-matrix0 (building-geometry structural-geometry &optional top-floor?)
  ;; TODO: there is no need of finding eccentricity
  (if top-floor?
      (report-section "Top Floor Mass Calculation")
      (report-section "Other Floors Mass Calculation"))
  (let* ((l (bg-l building-geometry))         ;; centre to centre distance betn columns in x dir 
	 (b (bg-b building-geometry))
	 (m (1+ (length l)))            ;; number of columns in x-dir
	 (n (1+ (length b)))            ;; number of columns in y-dir
	 (height (* (if top-floor? 1/2 1)
		    (bg-h building-geometry)))
	 (slab-thickness (sg-slab-thickness structural-geometry))
	 (beam-height (sg-beam-height structural-geometry))
	 (beam-width .300d0)
	 (cs (sg-column-size structural-geometry))
	 (wt (sg-wall-thickness structural-geometry))
	 ;; TODO: either take wall-height = height - beam-height and beam_z = beam-height - slab-thickness
	 ;;       or     take wall-height = height - (beam-height - slab-thickness) and beam_z = beam-height
	 (wall-height (- height beam-height))
	 
	 ;; 2kN/m2 live load and 1.5kN/m2 floor finish
	 ;; 1.5kN/m2 floor finish is decreased in walls
	 (column  (make-rect :l cs     :b cs     :h (- height slab-thickness) :density (/ 25000 9.81d0)
			     :area-load (if top-floor? 0d0 (/ -3500d0 9.81))))
	 (beam_x  (make-rect :l nil    :b beam-width :h (- beam-height slab-thickness)               :density (/ 25000 9.81d0)))
	 (beam_y  (make-rect :l beam-width :b nil    :h (- beam-height slab-thickness)               :density (/ 25000 9.81d0)))
	 (wall_y  (make-rect :l wt     :b nil    :h wall-height               :density (/ 20000 9.81d0)
			     :area-load (if top-floor? 0d0 (/ -3500d0 9.81))))
	 (wall_x  (make-rect :l nil    :b wt     :h wall-height               :density (/ 20000 9.81d0)
			     :area-load (if top-floor? 0d0 (/ -3500d0 9.81))))
	 (slab    (make-rect :l nil    :b nil    :h slab-thickness            :density (/ 25000 9.81d0)
			     :area-load (if top-floor? 0d0 (/ 3500d0 9.81))))
    		
	 ;;(column-stiffness (make-rect :l .300d0 :b .300d0 :mass 1))
	 
	 (s (make-moi-sums))
	 ;; (stiffness-s (make-moi-sums))
	 ;; stiffness
	 moi
	 )

    ;; Columns
    (report-subsection "Columns")
    (grid (x y)
	  :shift (0 0)
	  :size (m n)
	  :at (l b) :do
	  (add s           column           :x x :y y :base :bl)
	  ;;(add stiffness-s column-stiffness :x x :y y :base :bl)
	  )

    ;; Beams and Walls along the length (x - axis)
    (report-subsection "Beams and Walls (x-axis)")
    (grid (x y i j)
	  :shift ((rect-l column) 0)
	  :size ((1- m) n)
	  :at (l b) :do
	  (let ((y-offset (cond                            ;; if not outer walls align to centre of columns
			    ((= j 0)      0)
			    ((= j (1- n)) (offsety-of wall_x :from :top    :to :top    :of column))
			    (t            (offsety-of wall_x :from :centre :to :centre :of column)))))
	    
	    (add s beam_x :x x :y y :base :bl :l (- (aref l i) (rect-l column)))
	    (add s wall_x :x x :y (+ y y-offset) :base :bl :l (- (aref l i) (rect-l column)))
	    ))

    ;; Beams and Walls along the breadth (y - axis)
    (report-subsection "Beams and Walls (y-axis)")
    (grid (x y i j)
	  :shift (0 (rect-b column))
	  :size (m (1- n))
	  :at (l b) :do
	  (let ((x-offset (cond                            ;; if not outer walls align to centre of columns
			    ((= i 0)      0)
			    ((= i (1- m)) (offsetx-of wall_y :from :right  :to :right  :of column))
			    (t            (offsetx-of wall_y :from :centre :to :centre :of column)))))
	    
	    (add s beam_y :x x :y y :base :bl :b (- (aref b j) (rect-b column)))
	    (add s wall_y :x (+ x x-offset) :y y :base :bl :b (- (aref b j) (rect-b column)))
	    ))

    (report-subsection "Slab")
    (add s slab :x 0 :y 0 :base :bl :l (+ (rect-l column) (reduce #'+ l)) :b (+ (rect-b column) (reduce #'+ b)))

    ;; (setf moi (moi s)
    ;; 	  stiffness (moi stiffness-s))
    ;; (let ((ex (- (moi-xc moi) (moi-xc stiffness)))
    ;; 	  (ey (- (moi-yc moi) (moi-yc stiffness))))
    ;;   (values moi ex ey))))
    (setf moi (moi s))
    (report-subsection "Floor Mass Values")
    (report-values "" moi)
    (values moi)))

(defun mass-matrix (building-geometry structural-geometry)
  (let* ((n (bg-number-of-storey building-geometry))
	 (3n (* 3 n))
	 (M (zeros (list (* 3 n) (* 3 n))))
	 (moi (mass-matrix0 building-geometry structural-geometry))
	 (moi-top-floor (mass-matrix0 building-geometry structural-geometry t)))
    (loop for i from 0 to (* 3 (- n 2)) by 3 do
      (setf (aref M i i)             (moi-mass moi)
	    (aref M (1+ i) (1+ i))   (moi-mass moi)
	    (aref M (+ 2 i) (+ 2 i))  (moi-ipc  moi)))
    (setf (aref M (- 3n 1) (- 3n 1)) (moi-ipc moi-top-floor)
	  (aref M (- 3n 2) (- 3n 2)) (moi-mass moi-top-floor)
	  (aref M (- 3n 3) (- 3n 3)) (moi-mass moi-top-floor))
    (values M (moi-xc moi) (moi-yc moi)))) 

;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Stiffness Matrix
;;;;
;;;;;;;;;;;;;;;;;;;;

;;; Strut Modelling
(defun strut-dimensions (l b h Ic ec ew)
  "l is length, b is thickness, h is height of infill wall
returns length, thicknness and width of strut
Ec = E of column 
Ew = E of wall "
  (reporting-let*
   ((lds (sqrt (+ (expt l 2) (expt h 2))) "" :verbose)
    (theta (atan h l) "" :verbose)
    (alpha (* h (expt (/ (* Ew b (sin (* 2 theta)))
			 (* 4 Ec Ic h))
		      1/4)) "" :verbose)
    (wds (* 0.175 (expt alpha -0.4) lds) "" :verbose)
    (tds (max (/ h 12) (/ l 12) b) "" :verbose))
   (values lds tds wds theta)))

(defun infill-wall-stiffness (length thickness height Ic Ec Ew)
  (multiple-value-bind (lds tds wds theta) (strut-dimensions length thickness height Ic Ec Ew)
    (reporting-let*
	((k (* tds wds Ew (/ Lds) (expt (cos theta) 2))
	 "Strut Vertical Stiffness" :detailed))
     k)))					

(defun infill-walls-stiffness (lengths height Ic structural-geometry)
  "Totall k of walls along the direction of their lengths"
  (let ((column-width (sg-column-size structural-geometry))
	(beam-height (sg-beam-height structural-geometry))
	(thickness (sg-wall-thickness structural-geometry))
	(Ec (sg-column-elasticity structural-geometry))
	(Ew (sg-wall-elasticity structural-geometry)))
    (let ((k (loop for l across lengths
		   ;; NOTE : 0.3 = column-width, 0.5 = beam thickness
		   summing (infill-wall-stiffness (- l column-width) thickness (- height beam-height) Ic Ec Ew))))
      (reporting
       (format t "~%Infill Walls (single floor) K = ~,3f" k))
      k)))


;;; Coordinate Transforms
(defun local-to-global-k (local-k &key direction distance)
  "Convert local stiffness to global. 
`local-k' is n by n marix, that takes local displacement [x1,x2,..] or [y1,y2,...]
The local origin is at `distance'(+-) from origin of global coordinate to either `:x' or `:y' `direction'.
The `global-k' is 3n by 3n matrix that takes global displacement [x,y,theta, ...]"
  (let* ((n (array-dimension local-k 0))
	 (A (zeros (list n (* 3 n)))))
    ;; A converts from local coordinate to global coordinate
    (loop for i from 0 below n do
      (ecase direction
	(:y ;; A is transformation matrix x' = x - y theta
	 ;; [1 0 -y  0 0  0  0 0 ...
	 ;;  0 0  0  1 0 -y  0 0 ...
	 ;;  0 0  0  0 0  0  1 0 ...
	 ;;  . . . ]		  
	 (setf (aref A i (* 3 i)) 1
	       (aref A i (+ 2 (* 3 i))) (- distance)))
	(:x
	 ;; A is transformation matrix y' = y + x theta
	 ;; [0 1 x   0 0 0   0 0 ...
	 ;;  0 0 0   0 1 x   0 0 ...
	 ;;  0 0 0   0 0 0   0 1 ...
	 ;;  ... ]
	 (setf (aref A i (+ 1 (* 3 i))) 1
	       (aref A i (+ 2 (* 3 i))) distance))))
    (reporting :detailed
	       (format t "Transformation matrix for (distance = ~,3f, direction= ~a)" distance direction)
	       (pretty-print A))
    (reduce #'matmul (list (transpose A) local-k A))))

;;; Stiffness Matrix

(defun n-floors-k (n)
  "Stiffness matrix for n floors assuming each floor has same stiffness
	 [2,-1, 0, ..
	 -1, 2,-1, ..
	  0,-1, 2, ..
	  ....      1]"
  (let ((k (make-array (list n n))))
    ;; fill the matrix with coeffs of k for a single column frame of height h
    (loop for i from 0 below n do
      (loop for j from 0 below n do
	(setf (aref k i j)
	      (cond ((= i j)
		     (if (= j (1- n)) 1 2))
		    ((= (abs (- i j)) 1)
		     -1)
		    (t 0)))))
    k))

(defun stiffness-matrix (building-geometry structural-geometry &key xc yc (strut t))
  (report-section "Stiffness Matrix Calculation")
  (with-slots (number-of-storey l b (height h)) building-geometry
    (with-slots (column-size wall-thickness) structural-geometry 
      (reporting-let*
       ((m (1+ (length l)))
	(n (1+ (length b)))
	(nk (n-floors-k number-of-storey))
	(Ic (* 1/12 (expt column-size 4)) "MOI of Column") ;; NOTE: Assuming column is square
	(Ec (sg-column-elasticity structural-geometry))
	(Kc (* 12 Ec Ic (/ (expt height 3))) "K of Column (single floor)")
	(global-k (zeros (list (* 3 number-of-storey) (* 3 number-of-storey)))))
       ;; change stiffness from local to global

       (report-subsection "Frames and Infill Walls (along X-axis)")
       ;; frames along x-axis having m bays 
       (let ((local-k (mat* (+ (if strut ;; infill walls 
				   (infill-walls-stiffness l height Ic structural-geometry)
				   0) 
			       (* m Kc))                                    ;; columns
			    nK)))
	 (report-values "Local Stiffness" local-k)
	 (grid yf   ;; y coordinate of frames
	       :shift (+ (/ column-size 2) (- yc)) ;; change y coord relative to mass centre
	       :size n :at b :do
	       (mincf global-k (local-to-global-k local-k :direction :y :distance yf))))
       
       (report-subsection "Frames and Infill Walls (along Y-axis)")
       ;; frames along y-axis
       (let ((local-k (mat* (+ (if strut (infill-walls-stiffness b  height Ic structural-geometry) 0)   ;; infill walls 
			       (* n Kc))                                    ;; columns
			    nK)))
	 (report-values "Local Stiffness " local-k)
	 (grid xf
	       :shift (+ (/ column-size 2) (- xc)) 
	       :size m
	       :at l :do
	       (mincf global-k (local-to-global-k local-k
						  :direction :x :distance xf))))
       
       global-k))))


;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Everything Together
;;;;
;;;;;;;;;;;;;;;;;;;;

(defun test ()
  (let* ((geometry (make-building-geometry :number-of-storey 2
					   :l (vector 5 6 7)
					   :b (vector 4 7.6 9)
					   :h 3))
	 (structure (make-structural-geometry :column-size 0.3d0
					      :wall-thickness 0.230d0
					      :slab-thickness 0.15d0)))
    (report-section "Inputs")
    (report "" geometry structure)
    (multiple-value-bind (M xc yc) (mass-matrix geometry structure)
      (let ((K (stiffness-matrix geometry structure :xc xc :yc yc)))
	(report-section "Output")
	(report "" M K)
	(values M K)))))

(defun reporting-test () 
  (with-report-to-file ("/tmp/report.org" :verbose)
		       (test)))

;; With fundamental timeperiod 

(defun df-matrix (matrix)
  (let* ((dim (array-dimensions matrix))
	 (array (make-array (* (first dim) (second dim)) :initial-element 0.0d0 :element-type 'double-float)))
    (loop for i from 0 below (first dim) do 
      (loop for j from 0 below (second dim) do 
	(setf (aref array (+ i (* (first dim) j))) (coerce (aref matrix i j) 'double-float))))
    (magicl:from-array array dim)))

(defun timeperiods (eigenvalues)
  (sort (mapcar (lambda (omega^2) (/ (* 2 pi) (sqrt omega^2))) eigenvalues)
	'>))

(defun simple-building (&key number-of-storey height bays-x bays-y bay-width (strut t))
  (let* ((geometry (make-building-geometry :number-of-storey number-of-storey
					   :l (make-array bays-x :initial-element bay-width)
					   :b (make-array bays-y :initial-element bay-width)
					   :h height))
	 (structure (make-structural-geometry)))
    (multiple-value-bind (M xc yc) (mass-matrix geometry structure)
      (let* ((K (stiffness-matrix geometry structure :xc xc :yc yc :strut strut))
	     (mm (df-matrix m))
	     (kk (df-matrix k))
	     (eigenvalues (magicl:eig (magicl:@ (magicl:inv mm) kk)))
	     (tp (timeperiods eigenvalues)))
	(values mm kk (first tp) tp)))))


(defun simple-building-shapes (&key number-of-storey height bays-x bays-y bay-width (strut t))
  (let* ((geometry (make-building-geometry :number-of-storey number-of-storey
					   :l (make-array bays-x :initial-element bay-width)
					   :b (make-array bays-y :initial-element bay-width)
					   :h height))
	 (structure (make-structural-geometry)))
    (multiple-value-bind (M xc yc) (mass-matrix geometry structure)
      (let* ((K (stiffness-matrix geometry structure :xc xc :yc yc :strut strut))
	     (mm (df-matrix m))
	     (kk (df-matrix k)))
	(magicl:eig (magicl:@ (magicl:inv mm) kk))))))



(defun test2 () 
  (simple-building :number-of-storey 2
		   :height 3
		   :bays-x 2 :bays-y 2
		   :bay-width 3))

(defun timeperiod (M K)
  (let* ((mm (df-matrix m))
	 (kk (df-matrix k))
	 (eigenvalues (magicl:eig (magicl:@ (magicl:inv mm) kk)))
	 (tp (timeperiods eigenvalues)))
    (values mm kk (first tp) tp eigenvalues)))

(let ((hashtable (make-hash-table :test #'equal)))
  (defun simple-building-ftp* (&rest params &key number-of-storey height bays-x bays-y bay-width (strut t))
    (let ((ftp? (gethash params hashtable)))
      (if ftp?
	  ftp?
	  (setf (gethash params hashtable) (nth-value 2 (simple-building :number-of-storey number-of-storey
									 :height height
									 :bays-x bays-x
									 :bays-y bays-y
									 :bay-width bay-width
									 :strut strut))))))
  (defun simple-building-ftp*-clear ()
    (setf hashtable (make-hash-table :test #'equal))))


(let ((hashtable (make-hash-table :test #'equal)))
  (defun building (bg sg &key (strut t))
    (multiple-value-bind (M xc yc) (mass-matrix bg sg)
      (timeperiod M (stiffness-matrix bg sg :xc xc :yc yc :strut strut))))
  
  (defun building-ftp* (&rest params &key number-of-storey height l b (strut t))
    (let ((ftp? (gethash params hashtable)))
      (if ftp?
	  ftp?
	  (setf (gethash params hashtable) (nth-value 2 (building (make-building-geometry :number-of-storey number-of-storey
											  :l l
											  :b b
											  :h height)
								  (make-structural-geometry)
								  :strut strut))))))
  (defun building-ftp*-clear ()
    (setf hashtable (make-hash-table :test #'equal))))
