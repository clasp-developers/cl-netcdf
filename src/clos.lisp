(in-package :netcdf)

(defclass dimension ()
  ((dimid :initarg :dimid :accessor dimid)
   (name :initarg :name :accessor name)
   (len :initarg :len :accessor len)))

(defclass attribute ()
  ((xtype :initarg :xtype :accessor xtype)
   (len :initarg :len :accessor len)))

(defclass variable ()
  ((varid :initarg :varid :accessor varid)
   (xtype :initarg :xtype :accessor xtype)
   (ndims :initarg :ndims :accessor ndims)
   (dimids :initarg :dimids :accessor dimids)
   (natts :initarg :natts :accessor natts)))

(defclass netcdf ()
  ((id :initarg :id :accessor id)
   (dimensions :initarg :dimensions
               :initform (make-hash-table :test 'equal)
               :accessor dimensions)
   (variables :initform (make-hash-table :test 'equal)
              :initarg :variables
              :accessor variables)
   (global-attributes :initarg :global-attributes
                      :accessor global-attributes)
   ))

(defvar *xtype-alist* (list (cons netcdf-cffi:+nat+ :nat)
                            (cons netcdf-cffi:+byte+ :byte)
                            (cons netcdf-cffi:+char+ :char)
                            (cons netcdf-cffi:+short+ :short)
                            (cons netcdf-cffi:+int+ :int)
                            (cons netcdf-cffi:+float+ :float)
                            (cons netcdf-cffi:+double+ :double)
                            (cons netcdf-cffi:+ubyte+ :ubyte)
                            (cons netcdf-cffi:+ushort+ :ushort)
                            (cons netcdf-cffi:+uint+ :uint)
                            (cons netcdf-cffi:+int-64+ :int-64)
                            (cons netcdf-cffi:+uint-64+ :uint-64)
                            (cons netcdf-cffi:+string+ :string)))
                             
(defun put-dimension (netcdf name id)
  (setf (gethash name (slot-value netcdf 'dimensions)) id))

(defun get-dimension (netcdf name)
  (multiple-value-bind (value found)
      (gethash name (slot-value netcdf 'dimensions))
    (if found
        (len value)
        (error "%a non found dans les dimmensions" name)
        )))

(defun put-variable (netcdf name id)
  (setf (gethash name (slot-value netcdf 'variables)) id))

(defun get-variable (netcdf name)
  (multiple-value-bind (value found)
      (gethash name (slot-value netcdf 'variables))
    (if found
        (varid value)
        (error "~a not found in variables" name)
        )))

  

(defun create (path)
  (cffi:with-foreign-objects ((ncid :int))
    (nc-c:create path nc-c:+clobber+ ncid)
    (make-instance 'netcdf::netcdf :id (cffi:mem-ref ncid :int))))

(defun def-dim (cdf dim-name size)
  (cffi:with-foreign-objects ((dimid :int))
    (nc-c:def-dim (id cdf) dim-name (cffi:make-pointer size) dimid)
    (put-dimension cdf dim-name (cffi:mem-ref dimid :int))
    ))

(defun def-var (cdf var-name type dimensions)
  (cffi:with-foreign-objects ((varid :int)
                              (dimsid :int (length dimensions)))
    (loop for d in dimensions
       for i from 0
       do
         (setf (cffi:mem-aref dimsid :int i) (get-dimension cdf d)))
    (nc-c:def-var (id cdf) var-name type (length dimensions) dimsid varid)
    (netcdf::put-variable cdf var-name (cffi:mem-ref varid :int))
    ))


(defun copy-array-to-cffi (array cffi-pointer &optional (type :int))
  (loop for e across array
     for i from 0
     do
       (setf (cffi:mem-aref cffi-pointer type i)
             (aref array i))))

(defun copy-array-from-cffi (cffi-pointer array &optional (type :int))
  (loop for e across array
        for i from 0
        do
           (setf (aref array i)
                 (cffi:mem-aref cffi-pointer type i))))


(defun put-var-int (cdf var-name data)
  (cffi:with-foreign-objects ((cdata :int (first (array-dimensions data))))
    (nc:copy-array-to-cffi data cdata)
    (nc-c:put-var-int (id cdf) (get-variable cdf var-name)
                      cdata)))

(defun put-var-double (cdf var-name data)
  (cffi:with-foreign-objects ((cdata :double (first (array-dimensions data))))
    (nc:copy-array-to-cffi data cdata :double)
    (nc-c:put-var-double (id cdf) (get-variable cdf var-name)
                         cdata)))

(defun put-vara-int (cdf var-name start count data)
  (cffi:with-foreign-objects ((cdata :int64 (first (array-dimensions data)))
                              (ccount :int64 (first (array-dimensions count)))
                              (cstart :int64 (first (array-dimensions start))))
    (nc:copy-array-to-cffi data cdata)
    (nc:copy-array-to-cffi start cstart)
    (nc:copy-array-to-cffi count ccount)
    (nc-c:put-vara-int (id cdf) (get-variable cdf var-name)
                       cstart
                       ccount
                       cdata)))

(defun put-vara-double (cdf var-name start count data)
  (cffi:with-foreign-objects ((cdata :double (first (array-dimensions data)))
                              (ccount :int64 (first (array-dimensions count)))
                              (cstart :int64 (first (array-dimensions start))))
    (nc:copy-array-to-cffi data cdata :double)
    (nc:copy-array-to-cffi start cstart :int64)
    (nc:copy-array-to-cffi count ccount :int64)
    (nc-c:put-vara-double (id cdf) (get-variable cdf var-name)
                          cstart
                          ccount
                          cdata)))

(defun get-vara-double (cdf var-name start count data)
  (cffi:with-foreign-objects ((ccount :int64 (first (array-dimensions count)))
                              (cstart :int64 (first (array-dimensions start))))
    (copy-array-to-cffi start cstart :int64)
    (copy-array-to-cffi count ccount :int64)
    (nc-c:get-vara-double (id cdf) (get-variable cdf var-name)
                          cstart
                          ccount
                          (static-vectors:static-vector-pointer data))))

(defun get-vara-float (cdf var-name start count data)
  (cffi:with-foreign-objects ((ccount :int64 (first (array-dimensions count)))
                              (cstart :int64 (first (array-dimensions start))))
    (copy-array-to-cffi start cstart :int64)
    (copy-array-to-cffi count ccount :int64)
    (nc-c:get-vara-float (id cdf) (get-variable cdf var-name)
                         cstart
                         ccount
                         (static-vectors:static-vector-pointer data))))

(defun put-att-text (cdf var-name attribut value)
  (let ((var (if (eq nc-c:+global+ var-name)
                 nc-c:+global+
                 (get-variable cdf var-name))))
    (nc-c:put-att-text (id cdf) var attribut (cffi-sys:make-pointer (length value)) value)))


(defun enddef (cdf)
  (nc-c:enddef (id cdf)))

(defun nc-close (cdf)
  (nc-c:close (id cdf)))

(defmacro check-error (op)
  (let ((gsresult (gensym)))
    `(let ((,gsresult ,op))
       (unless (= ,gsresult 0)
         (error "When evaluating ~a result ~a" ',op ,gsresult)))))
       
(defun nc-inq (id)
  (cffi:with-foreign-objects ((ndimsp :int)
                              (nvarsp :int)
                              (nattsp :int)
                              (unlimdimidp :int))
    (check-error (nc-c:inq id ndimsp nvarsp nattsp unlimdimidp))
    (values (cffi:mem-ref ndimsp :int)
            (cffi:mem-ref nvarsp :int)
            (cffi:mem-ref nattsp :int)
            (cffi:mem-ref unlimdimidp :int))))

(defun inq-dim (id dimid)
  (cffi:with-foreign-object (lenp :int64)
    (let ((sname (cffi:with-foreign-pointer-as-string (sname (1+ netcdf-cffi:+max-name+))
                   (check-error (nc-c:inq-dim id dimid sname lenp)))))
      (values sname
              (make-instance 'dimension
                             :dimid dimid
                             :name sname
                             :len (cffi:mem-ref lenp :uint64))))))

(defun get-var-double (netcdf varid dest)
  (nc-c:get-var-double (id netcdf) varid dest))

(defun nc-inq-attid (netcdf name)
  (cffi:with-foreign-string (sname name)
    (cffi:with-foreign-object (varidp :int)
      (check-error (nc-c:inq-attid (id netcdf) -1 sname varidp))
      (cffi:mem-ref varidp :int))))

(defun nc-inq-attlen (netcdf varid name)
  (cffi:with-foreign-string (sname name)
    (cffi:with-foreign-object (lenp :int)
      (check-error (nc-c:inq-attlen (id netcdf) varid sname lenp))
      (cffi:mem-ref lenp :int))))

(defun nc-get-att-string (netcdf varid name)
  (cffi:with-foreign-string (sname name)
    (cffi:with-foreign-object (sptr :pointer)
      (check-error (nc-c:get-att-string (id netcdf) varid sname sptr))
      (cffi:foreign-string-to-lisp sptr))))

(defun nc-get-att-text (netcdf varid name)
  (cffi:with-foreign-string (sname name)
    (let ((slen (nc-inq-attlen netcdf varid name)))
      (cffi:with-foreign-object (str :char slen)
        (check-error (nc-c:get-att-text (id netcdf) varid sname str))
        (cffi:foreign-string-to-lisp str :count slen )))))

(defun nc-inq-attname (id varid attnum)
    (let ((sname (cffi:with-foreign-pointer-as-string (sname (1+ netcdf-cffi:+max-name+))
                   (check-error (nc-c:inq-attname id varid attnum sname)))))
      sname))

(defun nc-inq-att (id varid attnum)
  (cffi:with-foreign-objects ((xtypep :int)
                              (lenp :int))
    (let ((sname (nc-inq-attname id varid attnum)))
      (check-error (nc-c:inq-att id varid sname xtypep lenp))
      (values sname (make-instance 'attribute
                                   :xtype (cdr (assoc (cffi:mem-ref xtypep :int) *xtype-alist*))
                                   :len (cffi:mem-ref lenp :int))))))

(defun nc-inq-var (id varid)
  (cffi:with-foreign-objects ((xtypep :int)
                              (ndimsp :int)
                              (natsp :int))
    (let* ((sname (cffi:with-foreign-pointer-as-string (sname (1+ netcdf-cffi:+max-name+))
                    (check-error (nc-c:inq-var id varid sname xtypep ndimsp (cffi:null-pointer) natsp))))
           (ndims (cffi:mem-ref ndimsp :int)))
      (cffi:with-foreign-object (dimidsp :int ndims)
        (check-error (nc-c:inq-var id varid sname (cffi:null-pointer) (cffi:null-pointer) dimidsp (cffi:null-pointer)))
        (values sname (make-instance 'variable
                                     :varid varid
                                     :xtype (cdr (assoc (cffi:mem-ref xtypep :int) *xtype-alist*))
                                     :ndims ndims
                                     :dimids (cffi:foreign-array-to-lisp dimidsp (list :array :int ndims))
                                     :natts (cffi:mem-ref natsp :int)))))))

(defun read-dims (id ndims)
  (let ((dim-ht (make-hash-table :test #'equal)))
    (loop for dimid from 0 below ndims
          do (multiple-value-bind (name lenp)
                 (inq-dim id dimid)
               (setf (gethash name dim-ht) lenp)))
    dim-ht))

(defun read-variables (id nvars)
  (let ((vars-ht (make-hash-table :test #'equal)))
    (loop for varid from 0 below nvars
          do (multiple-value-bind (name variable)
                 (nc-inq-var id varid)
               (setf (gethash name vars-ht) variable)))
    vars-ht))

(defun read-global-attributes (id natts)
  (let ((atts-ht (make-hash-table :test #'equal)))
    (loop for attid from 0 below natts
          for attname = (nc-inq-attname id nc-c:+global+ attid)
          do (multiple-value-bind (name att)
                 (nc-inq-att id nc-c:+global+ attid)
               (setf (gethash name atts-ht) att)))
    atts-ht))

(defun nc-open (path &key (mode netcdf-cffi:+nowrite+))
  (cffi:with-foreign-objects ((ncid :int))
    (let ((true-path (probe-file path)))
      (unless true-path
        (error "Could not find file ~a to open it" path))
      (cffi:with-foreign-string (spath (namestring path))
        (let ((result (nc-c:open (namestring true-path) mode ncid)))
          (unless (= result 0)
            (error "Could not open netcdf file ~s result -> ~d~%" path result))
          (let ((id (cffi:mem-ref ncid :int)))
            (multiple-value-bind (ndims nvars natts unlimdimid)
                (nc-inq id)
              ;; (format t "natts -> ~a~%" natts)
              (make-instance 'netcdf::netcdf :id id
                                             :dimensions (read-dims id ndims)
                                             :variables (read-variables id nvars)
                                             :global-attributes (read-global-attributes id natts)))))))))


#+(or)
(defun triangulate (pointlist)
  "Triangule un ensemble de point.
POINTLIST un array de points de la forme #( X0 Y0 X1 Y1 X2 Y2)

Retour: tableau de points formant les triangles.  #( P1t0 P2t0 P3t0 P1t1 P2t1 P3t1 )
 "
  (cffi:with-foreign-objects ((in 'tri-c:triangulateio)
                              (out 'tri-c:triangulateio)
                              (vor 'tri-c:triangulateio)
                              (ptlist :double (array-total-size pointlist)))
    (setf (cffi:foreign-slot-value in 'tri-c:triangulateio 'tri-c:numberofpoints) (/ (array-total-size pointlist) 2))
    (setf (cffi:foreign-slot-value in 'tri-c:triangulateio 'tri-c:numberofsegments) 0)
    (setf (cffi:foreign-slot-value in 'tri-c:triangulateio 'tri-c:numberofholes) 0)
    (setf (cffi:foreign-slot-value in 'tri-c:triangulateio 'tri-c:numberofregions) 0)
    (copy-array-to-cffi pointlist  ptlist :double)
    (setf (cffi:foreign-slot-value in 'tri-c:triangulateio 'tri-c:pointlist) ptlist)

    (setf (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:edgelist) (cffi-sys:null-pointer))
    (setf (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:edgemarkerlist) (cffi-sys:null-pointer))
    (setf (cffi:foreign-slot-value vor 'tri-c:triangulateio 'tri-c:numberofpoints) 0)

    (tri-c:triangulate "z" in out vor)


    (let* ((nvertices (* (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:numberofcorners)
                         (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:numberoftriangles)))
           (vertices (make-array nvertices)))

      ;; (format T "corner:~a triangle:~a ~a~%"
      ;;            (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:numberofcorners)
      ;;            (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:numberoftriangles)
      ;;            nvertices)

      (dotimes (i nvertices)
        (setf (aref vertices i) (cffi:mem-aref (cffi:foreign-slot-value out 'tri-c:triangulateio 'tri-c:trianglelist) :int i))
        )
      vertices)
    ))


(pushnew :netcdf cl:*features*)
