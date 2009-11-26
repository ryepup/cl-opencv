(require :cffi)

(defpackage :opencv-test
    (:use :common-lisp :cffi))
   
(in-package :opencv-test)


#| 
(define-foreign-library libcv
  (:unix (:or "libcv.so"))
  (t (error "not implemented")))
(use-foreign-library libcv)
|#

(define-foreign-library libhighgui
  (:unix (:or "libhighgui.so"))
  (t (error "not implemented")))
(use-foreign-library libhighgui)

(defctype cv-capture :pointer)
(defctype ipl-image :pointer)


;; start capturing frames from camera: index = camera_index + domain_offset (CV_CAP_*)
;;CVAPI(CvCapture*) cvCreateCameraCapture( int index );
(defcfun "cvCreateCameraCapture" cv-capture (index :int))

;; create window
;;CVAPI(int) cvNamedWindow( const char* name, int flags CV_DEFAULT(CV_WINDOW_AUTOSIZE) );
(defcfun "cvNamedWindow" :int
  (name :string)
  (flags :int))

;; destroy window and all the trackers associated with it
;;CVAPI(void) cvDestroyWindow( const char* name );
(defcfun "cvDestroyWindow" :void
  (name :string))

;; Just a combination of cvGrabFrame and cvRetrieveFrame
;;   !!!DO NOT RELEASE or MODIFY the retrieved frame!!!
;;CVAPI(IplImage*) cvQueryFrame( CvCapture* capture );
(defcfun "cvQueryFrame" :pointer
  (capture cv-capture))

;; display image within window (highgui windows remember their content)
;;CVAPI(void) cvShowImage( const char* name, const CvArr* image );
(defcfun "cvShowImage" :void
  (window :string)
  (frame :pointer))

;; stop capturing/reading and free resources
;;CVAPI(void) cvReleaseCapture( CvCapture** capture );
(defcfun "cvReleaseCapture" :void
  (capture cv-capture))

(defun call-with-window (window-name body-fn)
  (unwind-protect
       (progn (cvnamedwindow window-name 1)
	      (funcall body-fn))
    (cvdestroywindow window-name)))

(defmacro with-window ((window-name) &body body)
  `(call-with-window ,window-name
		     #'(lambda () ,@body)))

(defvar *capture*)

(defun cam-passthrough (&optional (n 10))
  (with-window ("test")
    (setf *capture* (cvcreatecameracapture 0))
    (dotimes (i n)
      (let ((frame (cvqueryframe *capture*)))
	(break "~a" frame)
	(cvshowimage "test" frame )))
    (with-foreign-pointer (cap 1)
      (setf (mem-ref cap :pointer) *capture*)
      (cvreleasecapture cap))))
#|
  CvCapture* capture = cvCaptureFromCAM( CV_CAP_ANY );
  // Create a window in which the captured images will be presented
  cvNamedWindow( "mywindow", CV_WINDOW_AUTOSIZE );
  // Show the image captured from the camera in the window and repeat
  while( 1 ) {
    // Get one frame
    IplImage* frame = cvQueryFrame( capture );

    cvShowImage( "mywindow", frame );
    // Do not release the frame!

    //If ESC key pressed, Key=0x10001B under OpenCV 0.9.7(linux version),
    //remove higher bits using AND operator
    if( (cvWaitKey(10) & 255) == 27 ) break;
  }

  // Release the capture device housekeeping
  cvReleaseCapture( &capture );
  cvDestroyWindow( "mywindow" );
|#

