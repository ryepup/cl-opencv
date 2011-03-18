;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; imgproc.lisp
;;;; OpenCV bindings for SBCL
;;;; Image processing
(in-package :cl-opencv)


;;; Histograms



;;; Image Filtering

(defctype ipl-conv-kernel :pointer)

;; TODO test cvCopyMakeBorder
;; void cvCopyMakeBorder(const CvArr* src, CvArr* dst, CvPoint offset, 
;;                       int bordertype, CvScalar value = cvScalarAll(0))
(defanonenum
  +ipl-border-constant+
  +ipl-border-replicate+)

(defcfun ("cvCopyMakeBorder_glue" %copy-make-border) :void
  (src cv-array)
  (dest cv-array)
  (offset :int64)
  (border-type :int)
  (s1 :double)
  (s2 :double)
  (s3 :double)
  (s4 :double))

(defun copy-make-border (src dest offset border-type 
			 &optional (value (make-cv-scalar)))
  (apply #'%copy-make-border src dest offset border-type value))

;; IplConvKernel* 
;; cvCreateStructuringElementEx(int cols, int rows, int anchorX, int anchorY, 
;;                              int shape, int* values=NULL)
(defanonenum
  +cv-shape-rect+
  +cv-shape-cross+
  +cv-shape-ellipse+
  (+cv-shape-custom+ 100))

(defcfun ("cvCreateStructuringElementEx" %create-structuring-element-ex) :void
  (cols :int)
  (rows :int)
  (anchor-x :int)
  (anchor-y :int)
  (shape :int)
  (values :pointer))

;; TODO handle array of values in create-structuring-element-ex
(defun create-structuring-element-ex (cols rows anchor-x anchor-y shape 
				      &optional (values (null-pointer)))
  "Creates and fills an IplConvKernel structure. The structure will be
of dimensions COLSxROWS with the anchor at (ANCHOR-X, ANCHOR-Y) with
SHAPE filled with VALUES."
  (%create-structuring-element-ex cols rows anchor-x anchor-y shape values))

;; void cvDilate(const CvArr* src, CvArr* dst, IplConvKernel* element = NULL, 
;;               int iterations = 1)
(defcfun ("cvDilate" %dilate) :void
  (src cv-array)
  (dest cv-array)
  (element ipl-conv-kernel)
  (iterations :int))

(defun dilate (src dest &optional (element (null-pointer)) (iterations 1))
  (%dilate src dest element iterations))

;; void cvPyrDown(const CvArr* src, CvArr* dst, int filter=CV_GAUSSIAN_5x5)
(defanonenum
  (+gaussian-5x5+ 7))

(defcfun ("cvPyrDown" %pyr-down) :void
  (src cv-array)
  (dest cv-array)
  (filter :int))

(defun pyr-down (src dest &optional (filter +gaussian-5x5+))
  "Perform downsampling step of the Gaussian pyramid decomposition on
the image SRC and store it in DEST. Use the Gaussian filter type
FILTER for the convolution."
  (%pyr-down src dest filter))




;;; Geometric Image Transformations



;;; Miscellaneous Image Transformations

;; void cvAdaptiveThreshold(const CvArr* src, CvArr* dst, double maxValue, 
;;                          int adaptive_method=CV_ADAPTIVE_THRESH_MEAN_C, 
;;                          int thresholdType=CV_THRESH_BINARY, int blockSize=3,
;;                          double param1=5)

;; Adaptive threshold types
(defanonenum
  +adaptive-thresh-mean-c+
  +adaptive-thresh-gaussian-c+)

(defcfun ("cvAdaptiveThreshold" %adaptive-threshold) :void
  (src cv-array)                ; source image
  (dest cv-array)               ; destination image
  (max-value :double)           ; maximum value: binary and inverse binary
  (adaptive-method :int)        ; adaptive thresholding algorithm
  (threshold-type :int)         ; binary or inverse binary thresholding
  (block-size :int)             ; pixel neighborhood size for thresholding
  (param-1 :double))            ; method-dependent parameter

(defun adaptive-threshold (src dest max-value &optional 
			   (adaptive-method +adaptive-thresh-mean-c+)
			   (threshold-type +thresh-binary+) (block-size 3)
			   (param-1 5))
  (%adaptive-threshold src dest (coerce max-value 'double-float) adaptive-method
		       threshold-type block-size 
		       (coerce param-1 'double-float)))

;; double cvThreshold(const CvArr* src, CvArr* dst, double threshold, 
;;                    double maxValue, int thresholdType)

;; Enumeration of threshold types for cvThreshold
(defanonenum
  +thresh-binary+
  +thresh-binary-inv+
  +thresh-trunc+
  +thresh-tozero+
  +thresh-tozero-inv+)

(defcfun ("cvThreshold" %threshold) :double
  (src cv-array)
  (dest cv-array)
  (threshold :double)
  (max-value :double)
  (threshold-type :int))

(defun threshold (src dest threshold max-value threshold-type)
  "Applies a fixed-level threshold to array elements. SRC is the
source array and DEST is the target array. THRESHOLD is the threshold
value and MAX-VALUE is the 'on' value for binary
thresholding. THRESHOLD-TYPE is the type of thresholding to be done."
  (%threshold src dest (coerce threshold 'double-float)
	      (coerce max-value 'double-float) threshold-type))



;;; Structural Analysis and Shape Descriptors



;;; Planar Subdivisions



;;; Motion Analysis and Object Tracking



;;; Feature Detection



;;; Object Detection

