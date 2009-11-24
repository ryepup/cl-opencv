(require :cffi)

(defpackage :opencv-test
    (:use :common-lisp :cffi))
   
(in-package :opencv-test)


 
(define-foreign-library libcv
  (:unix (:or "libcv.so"))
  (t (error "not implemented")))
(use-foreign-library libcv)

(define-foreign-library libhighgui
  (:unix (:or "libhighgui.so"))
  (t (error "not implemented")))
(use-foreign-library libhighgui)

(defctype cv-capture :pointer)
(defctype ipl-image :pointer)

(defcfun "cvInitSystem" :int
  (argv :int)
  (argc :pointer))

(defcfun "cvCreateCameraCapture" cv-capture (index :int))

(defcfun "cvNamedWindow" :int
  (name :string)
  (flags :int))

(defcfun "cvDestroyWindow" :void
  (name :string))

(defcfun "cvQueryFrame" ipl-image
  (capture cv-capture))

(defcfun "cvShowImage" :void
  (window :string)
  (frame ipl-image))

(defcfun "cvReleaseCapture" :void
  (capture cv-capture))

(defun cam-passthrough (&optional (n 1000))
  (let ((c (cvcreatecameracapture 0))
	(name "test"))
    (if (null-pointer-p c)
	"no cam"
	(progn
	  (cvnamedwindow name 1)
	(dotimes (i n)
	  (cvshowimage name
		       (cvqueryframe c))
	  )
	(cvdestroywindow name)
	(cvreleasecapture (pointer-address c)))))
  )
#|
  CvCapture* capture = cvCaptureFromCAM( CV_CAP_ANY );
  if( !capture ) {
    fprintf( stderr, "ERROR: capture is NULL \n" );
    getchar();
    return -1;
  }

  // Create a window in which the captured images will be presented
  cvNamedWindow( "mywindow", CV_WINDOW_AUTOSIZE );

  // Show the image captured from the camera in the window and repeat
  while( 1 ) {
    // Get one frame
    IplImage* frame = cvQueryFrame( capture );
    if( !frame ) {
      fprintf( stderr, "ERROR: frame is null...\n" );
      getchar();
      break;
    }

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

