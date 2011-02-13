;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; highgui.lisp
;;;; OpenCV bindings for SBCL
;;;; High-level GUI and Media I/O
(in-package :cl-opencv)


;;; Types and structures

;; CvCapture
(defctype cv-capture :pointer)

;; CvVideoWriter
(defctype cv-video-writer :pointer)



;;; User Interface

;; Operation flags for cvConvertImage
(defanonenum
  (+cvtimg-flip+ 1)
  (+cvtimg-swap-rb+ 2))

;; cvConvertImage(const CvArr *src, CvArr *dst, int flags)
(defcfun ("cvConvertImage" %convert-image) :void
  (src ipl-image)
  (dest ipl-image)
  (flags :int))

(defun convert-image (src dest &optional (flags 0))
"Converts SRC to DEST with an optional vertical flip and/or red/blue channel
switch as determined by FLAGS."
  (%convert-image src dest flags))

;; TODO int cvCreateTrackbar(const char* trackbarName, 
;;                           const char* windowName, int* value, int count, 
;;                           CvTrackbarCallback onChange)

;; void cvDestroyAllWindows(void)
(defcfun ("cvDestroyAllWindows" destroy-all-windows) :void
  "Destroy all named windows and free their resources.")

;; void cvDestroyWindow(const char* name)
(defcfun ("cvDestroyWindow" destroy-window) :void
  "Destroy the named window with name NAME and free its resources."
  (name :string))

;; TODO int cvGetTrackbarPos(const char* trackbarName, const char* windowName)

;; TODO int cvInitSystem(int argc, char** argv)

;; void cvMoveWindow(const char* name, int x, int y)
(defcfun ("cvMoveWindow" move-window) :void
  "Sets the position of the window NAME to X, Y."
  (name :string)
  (x :int)
  (y :int))

;; int cvNamedWindow(const char* name, int flags)

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

;; void cvResizeWindow(const char* name, int width, int height)¶
(defcfun ("cvResizeWindow" resize-window) :void
  "Resize the window with name NAME to WIDTH by HEIGHT."
  (name :string)
  (width :int)
  (height :int))

;; TODO void cvSetMouseCallback(const char* windowName, 
;;                              CvMouseCallback onMouse, void* param=NULL)

;; TODO void cvSetTrackbarPos(const char* trackbarName, 
;;                            const char* windowName, int pos)

;; void cvShowImage(const char* name, const CvArr* image)
(defcfun ("cvShowImage" show-image) :void
  "Show the picture IMAGE in the named window NAME."
  (name :string)
  (image ipl-image))

;; int cvWaitKey(int delay=0)
(defcfun ("cvWaitKey" %wait-key) :int
  (delay :int))
 
(defun wait-key (&optional (delay 0))
  "Wait up to DELAY milliseconds for a key press. Return the key press
if any. If DELAY is zero, this function doesn't return until a key is
pressed."
  (%wait-key delay))




;;; Reading and Writing Images and Video

;; Color mode constants for cvLoadImage.
(defanonenum 
  (+load-image-unchanged+ -1)
  +load-image-grayscale+
  +load-image-color+
  +load-image-anydepth+
  (+load-image-anycolor+ 4))

(defcfun ("cvLoadImage" %load-image) ipl-image
  (filename :string)
  (is-color :int))

;; IplImage* cvLoadImage(const char* filename, int iscolor=CV_LOAD_IMAGE_COLOR)
(defun load-image (filename &optional (is-color +load-image-color+))
  "Load the image at path FILENAME using color options IS-COLOR."
  (%load-image filename is-color))

(defcfun ("cvLoadImageM" %load-image-m) cv-matrix
  (filename :string)
  (is-color :int))

;; CvMat* cvLoadImageM(const char* filename, int iscolor=CV_LOAD_IMAGE_COLOR)
(defun load-image-m (filename &optional (is-color +load-image-color+))
  "Load the image from FILENAME as a CvMat using IS-COLOR color options."
  (%load-image-m filename is-color))

;; int cvSaveImage(const char* filename, const CvArr* image)
(defcfun ("cvSaveImage" save-image) :int
  "Save the image in IMAGE into the file FILENAME."
  (filename :string)
  (image ipl-image))

;; CvCapture* cvCreateCameraCapture(int index)
(defcfun ("cvCreateCameraCapture" create-camera-capture) cv-capture
  "Capture a video stream from the camera with index INDEX."
  (index :int))

;; CvCapture* cvCreateFileCapture(const char* filename)
(defcfun ("cvCreateFileCapture" create-file-capture) cv-capture
  "Initializes capturing a video from the file FILENAME."
  (filename :string))

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

;; double cvGetCaptureProperty(CvCapture* capture, int property_id)
(defcfun ("cvGetCaptureProperty" get-capture-property) :double
  "Retrieves that value of property PROPERTY-ID from the capture
stream CAPTURE."
  (capture cv-capture)
  (property-id :int))

;; int cvGrabFrame(CvCapture* capture)
(defcfun ("cvGrabFrame" grab-frame) :int
  "Grabs a frame from the video capture stream CAPTURE. The image is
stored internally. Use RETRIEVE-FRAME to retrieve the grabbed frame."
  (capture cv-capture))

;; IplImage* cvQueryFrame(CvCapture* capture)
(defcfun ("cvQueryFrame" query-frame) ipl-image
  "Grab a frame from a video capture stream CAPTURE, decompress it and
return it."
  (capture cv-capture))

(defcfun ("cvReleaseCapture" %release-capture) :void
  (capture-ptr :pointer))

;; void cvReleaseCapture(CvCapture** capture)
(defun release-capture (capture)
  "Release the resources use by the capture stream CAPTURE."
  (with-foreign-object (capture-ptr :pointer)
    (setf (mem-ref capture-ptr :pointer) capture)
    (%release-capture capture-ptr)))

;; IplImage* cvRetrieveFrame(CvCapture* capture)
(defcfun ("cvRetrieveFrame" retrieve-frame) ipl-image
  "Returns a pointer to the last image grabbed from CAPTURE-SRC with
GRAB-FRAME."
  (capture cv-capture))

(defcfun ("cvSetCaptureProperty" %set-capture-property) :int
  (capture cv-capture)
  (property-id :int)
  (value :double))

;; int cvSetCaptureProperty(CvCapture* capture, int property_id, double value)
(defun set-capture-property (capture property-id value)
  "Sets the value of the property PROPERTY-ID of the capture source
CAPTURE to the value VALUE."
  (%set-capture-property capture property-id (coerce value 'double-float)))

;; TODO CV_FOURCC


;; TODO CvVideoWriter* cvCreateVideoWriter(const char* filename, int fourcc, 
;;                                         double fps, CvSize frame_size, 
;;                                         int is_color=1)

;; TODO void cvReleaseVideoWriter(CvVideoWriter** writer)

;; TODO int cvWriteFrame(CvVideoWriter* writer, const IplImage* image)

