;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; cl-opencv.lisp
;;;; OpenCV bindings for SBCL
;;;; Library loading and common code
(in-package :cl-opencv)

;;; Foreign library setup
(when (member :darwin cl:*features*)
  (pushnew #p"/opt/local/lib/" cffi:*foreign-library-directories*))

(cffi:define-foreign-library highgui
  (:darwin (:or "libopencv_highgui.2.2.0.dylib" "libopencv_highgui.dylib"))
  (:unix (:or "libhighgui.so.2.1.0" "libhighgui.so" ))
  (t (:default "libhighgui")))
(cffi:use-foreign-library highgui)

(cffi:define-foreign-library cl-opencv-glue
  (:darwin "libcl-opencv-glue.dylib")
  (:unix "libcl-opencv-glue.so")
  (t (:default "libcl-opencv-glue")))
(cffi:use-foreign-library cl-opencv-glue)

;;; General macros and functions
(defmacro defanonenum (&body enums)
  "Converts anonymous enums to Lisp constants."
  `(cl:progn ,@(cl:loop for value in enums
			for index = 0 then (cl:1+ index)
			when (cl:listp value) 
			do (cl:setf index (cl:second value)
				    value (cl:first value))
			collect `(cl:defconstant ,value ,index))))
