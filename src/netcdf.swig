%module netcdf

%feature("intern_function","1");
%feature("export");


%insert("lisphead") %{
 (defpackage :netcdf-cffi
        (:nicknames :nc-c)
	(:use :cffi))
 (in-package :netcdf-cffi)

 (define-foreign-library libnetcdf
    (:unix "libnetcdf.so")
    (t (:default "libnetcdf")))

 (use-foreign-library libnetcdf)

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t "")))
		 (name-list (cl:concatenate 'cl:list name)))
  	  (cl:when (cl:and (cl:char-equal #\N (cl:first name-list))
		     (cl:char-equal #\C (cl:second name-list))
		     (cl:char-equal #\_ (cl:third name-list)))
	    (cl:setq name-list (cl:cdddr name-list)))
        (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper name-list cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here

%}


//%ignore "_FillValue"
//%ignore "DIM_WITHOUT_VARIABLE"
%include "netcdf.h"
