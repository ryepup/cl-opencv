;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings for SBCL
;;;; Core functionality
(in-package :cl-opencv)


;;; Basic Structures

;; CvSize
;; We do some heinous kludging here for now. CFFI cannot pass structs by value,
;; which is almost always how OpenCV uses CvSize. We get around this by 
;; treating the CvSize struct as a 64-bit integer, which CFFI can handle (at
;; least on some platforms). The correct answer is either to write some glue
;; C code to translate structs passed by value to pointers or to figure out
;; how lispbuilder-sdl deals with the SDL_Color struct (it doesn't seem to 
;; use glue code).
(defstruct cv-size (width 0) (height 0))

(defun cv-size-to-int64 (s)
  "Packs the cv-size structure S into a 64-bit integer."
  (+ (cv-size-width s) (ash (cv-size-height s) 32)))

(defun int64-to-cv-size (n)
  "Unpacks the CvSize struct S from a 64-bit integer to a Lisp cv-size struct."
  (make-cv-size :width (logand n #x00000000ffffffff)
		:height (ash n -32)))

;; CvRect
;; More kludging Lisp structs to 64-bit integers which are really C structs.
(defstruct cv-rect (x 0) (y 0) (width 0) (height 0))

(defun cv-rect-to-int64s (r)
  "Convert a cv-rect struct R into two 64-bit integers."
  (let ((i1 (+ (cv-rect-x r) (ash (cv-rect-y r) 32)))
	(i2 (+ (cv-rect-width r) (ash (cv-rect-height r) 32))))
    (list i1 i2)))

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
  (let ((nsize (cv-size-to-int64 size)))
    (%create-image nsize depth channels)))

;; CvSize cvGetSize(const CvArr* arr)
(defcfun ("cvGetSize" %get-size) :int64
  (arr cv-array))

(defun get-size (arr)
  "Get the dimensions of the OpenCV array ARR. Return a cv-size struct with the
dimensions."
  (let ((nsize (%get-size arr)))
    (int64-to-cv-size nsize)))

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
  (let ((irect (cv-rect-to-int64s rect)))
    (%set-image-roi image (first irect) (second irect))))
