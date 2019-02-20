
(ql:quickload :netcdf)
(ql:quickload :metatilities)

(defun save-scatter-points (&key (path "/tmp") (file-name "dumps") (axes 2) (points '()) (data '()))
  (let ((cdf (nc:create (format nil "~a/~a.nc" path file-name)))
	(ar (make-array (* axes (length points))
			:initial-contents (mapcar #'(lambda (p) (coerce p 'double-float))
						  (metatilities:flatten points))))
	(triangles nil))

    (nc:def-dim cdf "axes" axes)
    (nc:def-dim cdf "points" (length points))
    (nc:def-dim cdf "time" nc-c:+unlimited+)
    (nc:def-var cdf "locations" nc-c:+float+ '("points" "axes"))

    (when (= axes 2)
      (setq triangles (nc:triangulate ar))
      (nc:def-dim cdf "tri" 3)
      (nc:def-dim cdf "ntriangles" (/ (first (array-dimensions triangles)) 3))
      (nc:def-var cdf "connections" nc-c:+int+ '("ntriangles" "tri")))

    (mapc #'(lambda (variable)
	      (let ((name (first variable)))
		(nc:def-var cdf name nc-c:+float+ '("time" "points"))
		(nc:put-att-text cdf name "field" (concatenate 'string name ", scalar,series"))
		(nc:put-att-text cdf name "positions" "locations")
		(when (= axes 2)
		  (nc:put-att-text cdf name "connections" "connections,triangles"))))
	  data)
    (nc:enddef cdf)

    (nc:put-var-double cdf "locations" ar)
    (when (= axes 2)
      (nc:put-var-int cdf "connections" triangles))

    (dolist (var data)
      (let ((name (first var)))
	(dolist (entry (rest var))
	  (let ((time (first entry))
		(value (second entry))
		(start (make-array 2))
		(count (make-array 2)))
	    (setf (aref start 0) time)
	    (setf (aref start 1) 0)
	    (setf (aref count 0) 1)
	    (setf (aref count 1) (length value))
	    (nc:put-vara-double cdf name
				start count
				(make-array (length value)
					    :initial-contents (mapcar #'(lambda (p) (coerce p 'double-float))
								      value)))))))
    (nc:nc-close cdf)))

B(defun run-example ()
  (example :points '((0.0 0.0) (2.0 4.0) (1.0 3.0) (3.0 2.0))
	   :data '(("a" (0 (1.0 2.0 3.0 4.0)) (1 (2.0 4.0 5.0 6.0)))
		   ("b" (0 (2.0 3.0 4.0 5.0)) (1 (4.0 5.0 6.0 2.0)))
		   )))
