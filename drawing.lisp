(in-package :moi)
(ql:quickload :sdl-gui-utils)

(defparameter *register-shape-for-drawing* nil)
(defparameter *shapes* nil)
(defparameter *translation* (sdl:point :x 0 :y 0))
(defparameter *scale* 1)

(defmethod add :after ((s moi-sums) (r rect) &key x y
											   (base :bl)
											   (l (slot-value r 'l))
											   (b (slot-value r 'b))
											   (mass (slot-value r 'mass)))
  (declare (ignore s mass))
  (flet ((select-color (min max)
		   (cond ((= min 0.3d0 max) sdl:*red*)
				 ((= min 0.3d0) sdl:*green*)
				 (t  sdl:*black*))))
	(when *register-shape-for-drawing*
	  (let (xc yc
			   (color (select-color (min l b) (max l b))))
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
		(let ((x0 (- xc (/ l 2)))
			  (y0 (- (- yc) (/ b 2))))
		  (push (list x0 y0 l b color) *shapes*))))))

(defun draw-shapes ()
  (let ((x (sdl:x *translation*))
		(y (sdl:y *translation*)))

	(loop for (x0 y0 l b color) in *shapes* do
	  (sdl:draw-rectangle-* (truncate (+ (* *scale*  x0) x))
							(truncate (+ (* *scale* y0) y))
							(truncate (* *scale* l))
							(truncate (* *scale* b)) :color color))))

(defmacro with-drawing (&body body)
  `(let* ((*register-shape-for-drawing* t)
		  (*shapes* nil)
		  (*translation* (sdl:point :x 221 :y 577))
		  (*scale* 40)
		  (evh (make-instance 'sdl-gui-utils::events-handler))
		  (drager-scaler (make-instance 'sdl-gui-utils::dragndropnscale
										:translation *translation*
										:scale *scale*
										:callback (lambda (translation scale)
													(setf *translation* translation
														  *scale* scale)))))
	 ,@body
	 (sdl-gui-utils::push-interface evh drager-scaler :hoverable? nil :drawable? nil)
	 (sdl-gui-utils::loop-with-events-to (evh)
	   (draw-shapes))))

;;;;;;;;;;;;;;;;;;;;
;;;; MAGIC
;;;; Test This
;;;;;;;;;;;;;;;;;;;;


#+nil(with-drawing
	   (with-reporting :default
		 (test)))
