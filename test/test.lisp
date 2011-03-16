(in-package #:cl-opencv-test)

;;various settings that depend on the camera in use
(defvar *default-width* 640)
(defvar *default-height* 480)
(defvar *frames-per-second* 30)
(defvar *millis-per-frame* (round (/ 1000 *frames-per-second*)))

(defun display (filename)
  "Open the image FILENAME and show it in a window."
  (let ((image (load-image filename 1)))
    (named-window "Display" 1)
    (show-image "Display" image)
    (loop while (not (= (wait-key 0) 27)))
    (release-image image)
    (destroy-window "Display")))

(defun show-camera (&optional (camera-index 0) (width *default-width*)
                    (height *default-height*))
  "Show the output from the camera CAMERA-INDEX."
  (with-capture (capture (create-camera-capture camera-index))
    (let ((window-name "Camera"))
      (set-capture-property capture +cap-prop-frame-width+ width)
      (set-capture-property capture +cap-prop-frame-height+ height)
      (named-window window-name)
      (do ((frame (query-frame capture) (query-frame capture)))
          ((plusp (wait-key *millis-per-frame*)) nil)
        (show-image window-name frame))
      (destroy-window window-name))))

(defun show-camera-threshold (&optional (camera-index 0)
                              (width *default-width*) (height *default-height*))
  "Show the camera output and a thresholded version in a single window."
  (with-capture (capture (create-camera-capture camera-index))
    (let* ((img-size (make-cv-size :width width :height height))         
           (window-name "Camera/Threshold")
           (grayscale (create-image img-size +ipl-depth-8u+ 1))
           (threshold (create-image img-size +ipl-depth-8u+ 1))
           (threshold3 (create-image img-size +ipl-depth-8u+ 3))
           (window (create-image (make-cv-size 
				  :width (* 2 (cv-size-width img-size))
				  :height (cv-size-height img-size))
				 +ipl-depth-8u+ 3))
           (cam-roi (make-cv-rect :x 0 :y 0 :width width :height height))
           (bw-roi (make-cv-rect :x width :y 0 :width width :height height)))
      (set-capture-property capture +cap-prop-frame-width+ 
			    (cv-size-width img-size))
      (set-capture-property capture +cap-prop-frame-height+ 
			    (cv-size-height img-size))
      (named-window window-name)
      (do ((frame (query-frame capture) (query-frame capture)))
          ((plusp (wait-key *millis-per-frame*)) nil)
        (set-image-roi window cam-roi)
        (copy frame window)
        (convert-image frame grayscale)
        (threshold grayscale threshold 128 255 +thresh-binary+)
        (convert-image threshold threshold3)
        (set-image-roi window bw-roi)
        (copy threshold3 window)
        (reset-image-roi window)
        (show-image window-name window))
      (destroy-window window-name)
      (release-image window)
      (release-image threshold3)
      (release-image threshold)
      (release-image grayscale))))
  
(defun camera-frame-diff (&optional (camera-index 0) (width *default-width*)
			 (height *default-height*))
  (with-capture (capture (create-camera-capture camera-index))
    (let* ((img-size (make-cv-size :width width :height height))
	   (window-name "Frame Difference")
	   (images (list (create-image img-size +ipl-depth-8u+ 1)
			 (create-image img-size +ipl-depth-8u+ 1)))
	   (dest (create-image img-size +ipl-depth-8u+ 1)))
      (set-capture-property capture +cap-prop-frame-width+ 
			    (cv-size-width img-size))
      (set-capture-property capture +cap-prop-frame-height+ 
			    (cv-size-height img-size))
      (named-window window-name)
      (do ((frame (query-frame capture) (query-frame capture))
	   (frame-num 0 (1+ frame-num)))
	  ((plusp (wait-key *millis-per-frame*)) nil)
        (convert-image frame (elt images (mod frame-num 2)))
	(abs-diff (first images) (second images) dest)
	(show-image window-name dest))
      (destroy-window window-name)
      (mapcar #'release-image images)
      (release-image dest))))

;; TODO fix camera-abs-diff
(defun camera-abs-diff (&optional (camera-index 0) (width *default-width*)
			 (height *default-height*))
  (with-capture (capture (create-camera-capture camera-index))
    (let* ((img-size (make-cv-size :width width :height height))
	   (window-name "Frame Absolute Difference")
	   (dest (create-image img-size +ipl-depth-8u+ 3))
	   (scalar (make-scalar 128.0 128.0 128.0)))
      (set-capture-property capture +cap-prop-frame-width+ 
			    (cv-size-width img-size))
      (set-capture-property capture +cap-prop-frame-height+ 
			    (cv-size-height img-size))
      (named-window window-name)
      (do ((frame (query-frame capture) (query-frame capture)))
	  ((plusp (wait-key *millis-per-frame*)) nil)
	(abs-diff-scalar frame dest scalar)
	(show-image window-name dest))
      (destroy-window window-name)
      (release-image dest))))
	 
(defun camera-subtract (&optional (camera-index 0) (width *default-width*)
			 (height *default-height*))
  (with-capture (capture (create-camera-capture camera-index))
    (let* ((img-size (make-cv-size :width width :height height))
	   (window-name "Frame Subtract")
	   (last-frame (create-image img-size +ipl-depth-8u+ 3))
	   (dest (create-image img-size +ipl-depth-8u+ 3)))
      (set-capture-property capture +cap-prop-frame-width+ 
			    (cv-size-width img-size))
      (set-capture-property capture +cap-prop-frame-height+ 
			    (cv-size-height img-size))
      (named-window window-name)
      (do ((frame (query-frame capture) (query-frame capture)))
	  ((plusp (wait-key *millis-per-frame*)) nil)
	(subtract frame last-frame dest)
	(show-image window-name dest)
	(copy frame last-frame))
      (destroy-window window-name)
      (release-image last-frame)
      (release-image dest))))

(defun show-video (filename)
  "Show the video in FILENAME in a window."
  (with-capture (capture (create-file-capture filename))
    (let ((width 
	   (truncate (get-capture-property capture +cap-prop-frame-width+)))
	  (height 
	   (truncate (get-capture-property capture +cap-prop-frame-height+)))
	  (frames 
	   (truncate (get-capture-property capture +cap-prop-frame-count+))))
      (format t "~a: ~:dx~:d ~:d frames~%" filename width height frames)
      (named-window filename)
      (do ((frame (query-frame capture) (query-frame capture)))
          ((or (plusp (wait-key *millis-per-frame*)) 
	       (cffi:null-pointer-p frame)) nil)
        (show-image filename frame))
      (destroy-window filename))))

(defun strip-summarize (filename)
  "Read a video from FILENAME and create a summary by taking a
vertical slice of pixels from the middle of each frame and creating an
image."
  (with-capture (capture (create-file-capture filename))
    (let* ((width 
	    (truncate (get-capture-property capture +cap-prop-frame-width+)))
	   (height 
	    (truncate (get-capture-property capture +cap-prop-frame-height+)))
	   (frames 
	    (truncate (get-capture-property capture +cap-prop-frame-count+)))
	   (x (/ width 2))
	   (frame-roi (make-cv-rect :x x :y 0 :width 1 :height height))
	   (img-size (make-cv-size :width frames :height height))
	   (img (create-image img-size +ipl-depth-8u+ 3)))
      (format t "~a: ~:dx~:d ~:d frames~%" filename width height frames)
      (format t "frame ROI: ~a~%" frame-roi)
      (format t "summary size: ~a~%" img-size)
      (named-window filename)
      (do* ((frame (query-frame capture) (query-frame capture))
	    (frame-num 0 (1+ frame-num))
	    (img-roi (make-cv-rect :width 1 :height height)
		     (make-cv-rect :x frame-num :width 1 :height height)))
	   ((cffi:null-pointer-p frame) nil)
	(reset-image-roi img)
	(show-image filename img)
	(set-image-roi frame frame-roi)
	(set-image-roi img img-roi)
	(copy frame img))
      (reset-image-roi img)
      (save-image (concatenate 'string filename ".tif") img)
      ;(release-image img)
      (destroy-window filename)
      img)))
      
      

	   
	   
