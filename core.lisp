;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings for SBCL
;;;; Core functionality
(in-package :cl-opencv)


;;; Basic Structures

;; TODO CvSize

;; CvMat
(defctype cv-matrix :pointer)

;; IplImage
(defctype ipl-image :pointer)

;; CvArr
(defctype cv-array :pointer)




;;; Operations on Arrays

(defcfun ("cvReleaseImage" %release-image) :void
  (image-ptr :pointer))

(defun release-image (image)
  "Release the resources use by the image held in IMAGE."
  (with-foreign-object (image-ptr :pointer)
    (setf (mem-ref image-ptr :pointer) image)
    (%release-image image-ptr)))


