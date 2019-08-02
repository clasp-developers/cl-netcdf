
(ql:quickload :netcdf)

(setf *default-pathname-defaults* (translate-logical-pathname #P"~/quicklisp/local-projects/cl-netcdf/examples/"))

(defparameter *n* (netcdf::nc-open #P"heat.rst7" :mode netcdf-cffi:+nowrite+))

(describe *n*)
(describe (netcdf:global-attributes *n*))
(describe (netcdf:variables *n*))
(netcdf::nc-get-att-string *n* 0 "ConventionVersion")

(apropos "static")
(defparameter *s* (static-vectors:make-static-vector 3 :element-type 'double-float :initial-contents #(1.0d 2.0d 3.0d)))

(netcdf::get-vara-double *n* "cell_lengths" (vector 0) (vector 3) *s*)
*s*


