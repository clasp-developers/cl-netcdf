
(ql:quickload :netcdf)


(setf *default-pathname-defaults* (translate-logical-pathname #P"~/quicklisp/local-projects/cl-netcdf/examples/"))

(probe-file "heat.rst7")

(defparameter *n* (netcdf::nc-open #P"heat.rst7" :mode netcdf-cffi:+nowrite+))

(defparameter *n* (netcdf::nc-open (probe-file "/Users/meister/Development/fep-benchmark/cando-fep-benchmarks/eg5/eg5-jobs/CHEMBL1088740-CHEMBL1077227/ligand/vdw-bonded/0.400/heat.rst7")))

(ext:getcwd)
