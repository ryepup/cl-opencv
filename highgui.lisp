;;; -*- mode: lisp; indent-tabs: nil -*-
;;; cl-opencv.lisp
;;; OpenCV bindings for SBCL
;;;
(in-package :cl-opencv)

(when (member :darwin cl:*features*)
  (pushnew #p"/opt/local/lib/" cffi:*foreign-library-directories*))

(define-foreign-library highgui
  (:darwin (:or "libopencv_highgui.2.2.0.dylib" "libopencv_highgui.dylib"))
    (:unix (:or "libhighgui.so.2.1.0" "libhighgui.so"))
  (t (:default "libhighgui")))

(use-foreign-library highgui)

(defctype capture :pointer)
(defctype ipl-image :pointer)

(defmacro defanonenum (&body enums)
  "Converts anonymous enums to Lisp constants."
  `(cl:progn ,@(cl:loop for value in enums
			for index = 0 then (cl:1+ index)
			when (cl:listp value) 
			do (cl:setf index (cl:second value)
				    value (cl:first value))
			collect `(cl:defconstant ,value ,index))))

;; Window constants for cvNamedWindow.
(defanonenum
    +window-normal+
    +window-autosize+)

(defcfun ("cvNamedWindow" %named-window) :int
  "Internal helper function for NAMED-WINDOW."
  (name :string)
  (flags :int))

(defun named-window (name &optional (flags +window-autosize+))
  "Create a window named NAME size according to
FLAGS. +WINDOW-AUTOSIZE+ sizes the window according to its
contents. Note that current OpenCV only supports +WINDOW-AUTOSIZE+."
  (%named-window name flags))

(defcfun ("cvDestroyWindow" destroy-window) :void
  "Destroy the named window with name NAME and free its resources."
  (name :string))

(defcfun ("cvDestroyAllWindows" destroy-all-windows) :void
  "Destroy all named windows and free their resources.")

;; Color mode constants for cvLoadImage.
(defanonenum 
  (+load-image-unchanged+ -1)
  +load-image-grayscale+
  +load-image-color+
  +load-image-anydepth+
  (+load-image-anycolor+ 4))

(defcfun ("cvLoadImage" load-image) ipl-image
  "Load the image at path FILENAME using color options IS-COLOR."
  (filename :string)
  (is-color :int))

(defcfun ("cvReleaseImage" %release-image) :void
  (image-ptr :pointer))

(defun release-image (image)
  "Release the resources use by the image held in IMAGE."
  (with-foreign-object (image-ptr :pointer)
    (setf (mem-ref image-ptr :pointer) image)
    (%release-image image-ptr)))

(defcfun ("cvShowImage" show-image) :void
  "Show the picture IMAGE in the named window NAME."
  (name :string)
  (image ipl-image))

(defcfun ("cvWaitKey" wait-key) :int
  "Wait up to DELAY milliseconds for a key press. Return the key press
if any. If DELAY is zero, this function doesn't return until a key is
pressed."
  (delay :int))
 
(defcfun ("cvCreateCameraCapture" create-camera-capture) capture
  "Capture a video stream from a camera."
  (index :int))

(defcfun ("cvGrabFrame" grab-frame) :int
  "Grabs a frame from the video capture stream CAPTURE-SRC. The image is
stored internally. Use RETRIEVE-FRAME to retrieve the grabbed frame."
  (capture-src capture))

(defcfun ("cvRetrieveFrame" retrieve-frame) ipl-image
  "Returns a pointer to the last image grabbed from CAPTURE-SRC with
GRAB-FRAME."
  (capture-src capture))


(defcfun ("cvQueryFrame" query-frame) ipl-image
  "Grab a frame from a video capture stream CAPTURE, decompress it and
return it."
  (capture-src capture))

(defcfun ("cvReleaseCapture" %release-capture) :void
  (image-ptr :pointer))

(defun release-capture (capture-src)
  "Release the resources use by the capture stream CAPTURE-SRC."
  (with-foreign-object (capture-ptr :pointer)
    (setf (mem-ref capture-ptr :pointer) capture-src)
    (%release-capture capture-ptr)))

;; Constants for cvSetCaptureProperty and cvGetCaptureProperty.
(defanonenum
  +cap-prop-pos-msec+      ; video position in milliseconds or capture timestamp
  +cap-prop-pos-frames+    ; 0-based index of frame to be decoded/captures next
  +cap-prop-pos-avi-ratio+ ; relative position of video file (0 to 1).
  +cap-prop-frame-width+   ; width of frames in the video stream
  +cap-prop-frame-height+  ; height of frames in the video stream
  +cap-prop-fps+           ; frame rate
  +cap-prop-fourcc+        ; 4-character code of the codec
  +cap-prop-frame-count+   ; number of frames in video file
  +cap-prop-format+        ; format of Mat objects returned by retrieve
  +cap-prop-mode+          ; backend-specific value indicating capture mode
  +cap-prop-brightness+    ; brightness of the image (only cameras)
  +cap-prop-contrast+      ; contrast of the image (only cameras)
  +cap-prop-saturation+    ; saturation of the image (only cameras)
  +cap-prop-hue+           ; hue of the image (only cameras)
  +cap-prop-gain+          ; gain of the image (only cameras)
  +cap-prop-exposure+      ; exposure of the image (only cameras)
  +cap-prop-convert-rgb+   ; indicates whether images should be converted to RGB
  +cap-prop-white-balance+ ; currently unsupported
  +cap-prop-rectification+); ? (only supported by DC1394 v 2.x backend)

(defcfun ("cvSetCaptureProperty" %set-capture-property) :int
  "Sets the value of the property PROPERTY-ID from the
capture stream CAPTURE-SRC to VALUE."
  (capture-src capture)
  (property-id :int)
  (value :double))

(defun set-capture-property (capture-src property-id value)
  "Sets the value of the property PROPERTY-ID of the capture source
CAPTURE-SRC to the value VALUE."
  (%set-capture-property capture-src property-id (coerce value 'double-float)))

(defcfun ("cvGetCaptureProperty" get-capture-property) :double
  "Retrieves that value of property PROPERTY-ID from the capture
stream CAPTURE-SRC."
  (capture-src capture)
  (property-id :int))
