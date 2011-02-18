(in-package #:cl-opencv-test)

;;various settings that depend on the camera in use
(defvar *default-width* 320)
(defvar *default-height* 240)
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
  
        