(in-package #:moi)

(defparameter *reporting-level* :none
  ":none :default, :detailed, :verbose, :vverbose")

(defun report? (&optional (level :default))
  "`level' is *reporting-level* keyword"
  (ecase level
	(:vverbose (eql *reporting-level* :vverbose))
	(:verbose (or (eql *reporting-level* :verbose)
				  (eql *reporting-level* :verbose)))
	(:detailed (or (eql *reporting-level* :detailed)
				   (eql *reporting-level* :vverbose)
				   (eql *reporting-level* :verbose)))
	(:default (not (eql *reporting-level* :none)))
	(:none nil)))

(defparameter *reporting-stream* *standard-output*)

(defun mprint(mat)
  (format *reporting-stream* "~%")
  (loop for i from 0 below (array-dimension mat 0) do
	(loop for j from 0 below (array-dimension mat 1) do
	  (format *reporting-stream* "| ~8f  " (aref mat i j)))
	(format *reporting-stream* "| ~%")))

(defun pretty-print (value)
  "Pretty print"
  (if (arrayp value)
	  (mprint value)
	  (print value *reporting-stream*)))

(defmacro report-values (title &rest values)
  (if values
	  `(when (report?)
		 (format *reporting-stream* "~%~a" ,title)
		 ,@(loop for v in values 
				 collect `(progn
							(pretty-print ,v))))
	  `(when (report?) (format *reporting-stream* "~%~a" ,title))))

(defmacro report (title &rest values)
  (if values
	  `(when (report?)
		 ,(if (or (not (stringp title))
				  (not (string-equal title "")))
			  `(format *reporting-stream* "~%~a" ,title))
		 ,@(loop for v in values 
				 collect `(progn
							(format *reporting-stream* "~%~a =" ',v)
							(pretty-print ,v))))
	  `(when (report?) (format *reporting-stream* "~%~a" ,title))))

(defun report-section (title)
  (when (report?) (format *reporting-stream* "~%* ~a~%" title)))

(defun report-subsection (title)
  (when (report?) (format *reporting-stream* "~%** ~a~%" title)))


(defmacro reporting (&rest body)
  "Execute the body if reporting is on"
  (let ((reporting-level :default))
	(if (or (keywordp (first body))
			(numberp (first body)))
		(setf reporting-level (first body)))
	`(if (report? ,reporting-level)
		 (let ((*standard-output* *reporting-stream*))
		   (progn
			 ,@body)))))

(defmacro report-let% (binding)
  (let ((var (first binding))
		(rhs (second binding))
		(name (third binding))
		(reporting-level (or (fourth binding) :default)))
	`(if (report? ,reporting-level)
		   (format *reporting-stream* "~%+ ~a (~a)= ~a = ~a" ,name ',var ',rhs ,var))))

(defmacro reporting-let* (bindings &rest body)
  (let (reports clean-bindings)
	(loop for b in bindings do
	  (when (and (listp b) (> (length b) 2))
		(push `(report-let% ,b)
			  reports))
	  (push (if (listp b)
				(list (first b) (second b))
				b)
			clean-bindings))
	`(let* ,(reverse clean-bindings)
	   ,@(reverse reports)
	   ,@body)))

(defmacro with-report-to-file ((file reporting-level &rest keys) &rest body)
  (alexandria:once-only (file)
	`(with-open-file (*reporting-stream* ,file :direction :output
											   :if-exists :supersede ,@keys)
	   (if (alexandria:ends-with-subseq ".org" ,file :test #'char-equal)
		   (format *reporting-stream* "#+OPTIONS: \\n:t~%"))
	   (let ((*reporting-level* ,reporting-level))
		 ,@body))))

