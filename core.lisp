;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings for SBCL
;;;; Core functionality
(in-package :cl-opencv)


;;; Basic Structures

;; We do some heinous kludging here for now. CFFI cannot pass structs
;; by value, which is almost always how OpenCV uses CvSize and
;; CvPoint. We get around this by treating the these structs as a
;; 64-bit integer, which CFFI can handle (at least on some
;; platforms). The correct answer is to write some glue C code to
;; translate structs passed by value to pointers.

(defmacro make-structure-serializers (struct slot1 slot2)
  "Create a serialization and deserialization functionf for the
structure STRUCT with integer slots SLOT1 and SLOT2. These functions
will pack and unpack the structure into an INT64."
  (let ((pack-fn (intern (concatenate 'string (string struct) 
				      (string '->int64))))
	(slot1-fn (intern (concatenate 'string (string struct) "-" 
				       (string slot1))))
	(slot2-fn (intern (concatenate 'string (string struct) "-" 
				       (string slot2))))
	(unpack-fn (intern (concatenate 'string (string 'int64->) 
					(string struct))))
	(make-fn (intern (concatenate 'string (string 'make-) 
				      (string struct)))))
    `(progn
       (defun ,pack-fn (s)
	 (+ (,slot1-fn s) (ash (,slot2-fn s) 32)))
       (defun ,unpack-fn (n)
	 (,make-fn ,slot1 (logand n #x00000000ffffffff)
		   ,slot2 (ash n -32))))))

;; CvPoint
(defstruct cv-point (x 0) (y 0))
(make-structure-serializers :cv-point :x :y)

;; CvSize
(defstruct cv-size (width 0) (height 0))
(make-structure-serializers :cv-size :width :height)

;; CvRect
;; More kludging Lisp structs to 64-bit integers which are really C structs.
(defstruct cv-rect (x 0) (y 0) (width 0) (height 0))

(defun cv-rect->int64s (r)
  "Convert a cv-rect struct R into two 64-bit integers."
  (let ((i1 (+ (cv-rect-x r) (ash (cv-rect-y r) 32)))
	(i2 (+ (cv-rect-width r) (ash (cv-rect-height r) 32))))
    (list i1 i2)))

;; CvScalar 
;; In OpenCV a scalar is a struct whose single member is an array of
;; four double values. We just use a list with values coerced to
;; double. We provide a helper function to create a Lisp version of a
;; scalar. The helper ensures that there are only four values in the
;; scalar.
(defun make-cv-scalar (&rest args)
  (mapcar #'(lambda (v) (coerce v 'double-float))
	  (cond ((> (length args) 4) (subseq args 0 4))
		((< (length args) 4) 
		 (do ((new-args (reverse args)))
		     ((= (length new-args) 4) (reverse new-args))
		   (push 0 new-args)))
		(t args))))

;; CvMat
(defctype cv-matrix :pointer)

;; IplImage
(defanonenum
  (+ipl-depth-1u+ 1)
  (+ipl-depth-8u+ 8)
  (+ipl-depth-16u+ 16)
  (+ipl-depth-32f+ 32)
  (+ipl-depth-64f+ 64)
  (+ipl-depth-8s+ #x80000008)
  (+ipl-depth-16s+ #x80000010)
  (+ipl-depth-32s+ #x80000020))

(defctype ipl-image :pointer)

;; CvArr
(defctype cv-array :pointer)




;;; Operations on Arrays

;; void cvAbsDiff(const CvArr* src1, const CvArr* src2, CvArr* dst)
(defcfun ("cvAbsDiff" abs-diff) :void
  "Calculate the absolute difference between elements in SRC1 and SRC2
and store them in DEST."
  (src1 cv-array)
  (src2 cv-array)
  (dest cv-array))

;; void cvAbsDiffS(const CvArr* src, CvArr* dst, CvScalar value)
(defcfun ("cvAbsDiffS_glue" %abs-diff-scalar-glue) :void
  (src cv-array)
  (dest cv-array)
  (s1 :double)
  (s2 :double)
  (s3 :double)
  (s4 :double))

(defun abs-diff-scalar (src dest scalar)
  "Calculate the absolute difference between elements of SRC and a fixed vector of values SCALAR. Store the result in DEST."
  (apply #'%abs-diff-scalar-glue src dest scalar))

;; void cvCopy(const CvArr* src, CvArr* dst, const CvArr* mask=NULL)
(defcfun ("cvCopy" %copy) :void
  (src cv-array)
  (dest cv-array)
  (mask cv-array))

(defun copy (src dest &optional (mask (null-pointer)))
  "Copy an image from SRC to DEST using MASK to determine which pixels
are copied."
  (%copy src dest mask))

;; IplImage* cvCreateImage(CvSize size, int depth, int channels)
(defcfun ("cvCreateImage" %create-image) ipl-image
  (size :int64)
  (depth :int)
  (channels :int))

(defun create-image (size depth channels)
  "Create an image with dimensions given by SIZE, DEPTH bits per
channel, and CHANNELS number of channels."
  (let ((nsize (cv-size->int64 size)))
    (%create-image nsize depth channels)))

;; CvSize cvGetSize(const CvArr* arr)
(defcfun ("cvGetSize" %get-size) :int64
  (arr cv-array))

(defun get-size (arr)
  "Get the dimensions of the OpenCV array ARR. Return a cv-size struct with the
dimensions."
  (let ((nsize (%get-size arr)))
    (int64->cv-size nsize)))

;; void cvReleaseImage(IplImage** image)
(defcfun ("cvReleaseImage" %release-image) :void
  (image-ptr :pointer))

(defun release-image (image)
  "Release the resources use by the image held in IMAGE."
  (with-foreign-object (image-ptr :pointer)
    (setf (mem-ref image-ptr :pointer) image)
    (%release-image image-ptr)))

;; void cvResetImageROI(IplImage* image)
(defcfun ("cvResetImageROI" reset-image-roi) :void
  "Reset the ROI for IMAGE."
  (image ipl-image))

;; void cvSetImageROI(IplImage* image, CvRect rect)
;; Note: the two int64 parameters actually represent a CvRect structure.
(defcfun ("cvSetImageROI" %set-image-roi) :void
  (image ipl-image)
  (rect-i1 :int64)
  (rect-i2 :int64))

(defun set-image-roi (image rect)
  "Set the ROI of IMAGE to the rectangle RECT."
  (let ((irect (cv-rect->int64s rect)))
    (%set-image-roi image (first irect) (second irect))))

;; void cvSub(const CvArr* src1, const CvArr* src2, CvArr* dst, 
;;            const CvArr* mask=NULL)
(defcfun ("cvSub" %subtract) :void
  (src1 cv-array)
  (src2 cv-array)
  (dest cv-array)
  (mask cv-array))

(defun subtract (src1 src2 dest &optional (mask (null-pointer)))
  "Subtract elements of SRC2 from SRC1 for the set bits in MASK and
store the result in DEST. This operation is saturating for types with
limited range."
  (%subtract src1 src2 dest mask))

;; void cvSubS(const CvArr* src, CvScalar value, CvArr* dst, 
;;             const CvArr* mask=NULL)
(defcfun ("cvSubS_glue" %subtract-scalar-glue) :void
  (src cv-array)
  (s1 :double)
  (s2 :double)
  (s3 :double)
  (s4 :double)
  (dest cv-array)
  (mask cv-array))

(defun subtract-scalar (src scalar dest &optional (mask (null-pointer)))
  "Subtract corresponding elements of SCALAR from each pixel in SRC
and store the result in DEST. Only subtract where pixels in MASK are
non-zero."
  (%subtract-scalar-glue src (first scalar) (second scalar) (third scalar) 
			 (fourth scalar) dest mask))

;; void cvSubRS(const CvArr* src, CvScalar value, CvArr* dst, 
;;              const CvArr* mask=NULL) 
(defcfun ("cvSubRS_glue" %subtract-r-scalar-glue) :void
  (src cv-array)
  (s1 :double)
  (s2 :double)
  (s3 :double)
  (s4 :double)
  (dest cv-array)
  (mask cv-array))
    
(defun subtract-r-scalar (src scalar dest &optional (mask (null-pointer)))
  "Subtract corresponding elements of SRC pixels from each element of
SCALAR and store the result in DEST. Only subtract where pixels in
MASK are non-zero."
  (%subtract-r-scalar-glue src (first scalar) (second scalar) (third scalar) 
			   (fourth scalar) dest mask))

  