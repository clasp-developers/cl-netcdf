(defpackage :netcdf-cffi
        (:nicknames :nc-c)
        (:use :cffi))

(defpackage :netcdf
  (:nicknames :nc)
  (:use :cl)
  (:export
   nc-open
   nc-close
   get-dimension
   copy-array-to-cffi
   copy-array-from-cffi
   create
   def-dim
   def-var
   enddef
   put-var-int
   put-var-double
   put-vara-int
   put-vara-double
   put-att-text
   #:xtype
   #:len
   #:ndims
   #:dimids
   #:natts
   #:attribute
   #:variable
   #:id
   #:dimensions
   #:variables
   #:global-attributes
   #:len
   #:netcdf
   #:varid
   #:get-vara-double-static
   #:get-vara-float
   #:get-vara-double
   ))

