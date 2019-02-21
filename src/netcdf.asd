;; -*- coding: utf-8; -*-
(in-package :asdf)

(defsystem "netcdf"
  :description ""
  :author       "Valvassori Moïse <moise@valvassori.org>"
  :licence "None"
  :depends-on (:cffi :static-vectors)
  :components (
               (:file "packages")
	       (:file "netcdf")
	       #+(or)(:file "triangle")
	       (:file "clos" :depends-on ("netcdf" #+(or)"triangle"))
	       ))
