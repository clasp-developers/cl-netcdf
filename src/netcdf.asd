(defpackage :netcdf-asdf
  (:use :asdf))

(in-package :netcdf-asdf)

(defsystem "netcdf"
  :description ""
  :author       "Valvassori Mo�se <moise@valvassori.org>"
  :licence "None"
  :depends-on (:cffi)
  :components (
	       (:file "netcdf")
	       ))
