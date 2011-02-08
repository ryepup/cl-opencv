(in-package #:cl-opencv-test)

(defun display (filename)
  "Open the image FILENAME and show it in a window."
  (let ((image (cl-opencv:load-image filename 1)))
    (cl-opencv:named-window "Display" 1)
    (cl-opencv:show-image "Display" image)
    (loop while
	 (not (= (cl-opencv:wait-key 0) 27)))
    (cl-opencv:release-image image)
    (cl-opencv:destroy-window "Display")))

(defun show-camera (&optional (camera-index 0))
  "Show the output from the camera CAMERA-INDEX."
  (let ((capture (cl-opencv:create-camera-capture camera-index))
	(window-name "Camera")
	(frame nil))
    (cl-opencv:set-capture-property capture +cap-prop-frame-width+ 640)
    (cl-opencv:set-capture-property capture +cap-prop-frame-height+ 480)
    (cl-opencv:named-window window-name)
    (do ((frame (cl-opencv:query-frame capture) 
		(cl-opencv:query-frame capture)))
	((= 27 (cl-opencv:wait-key 33)) nil)
      (cl-opencv:show-image window-name frame))
    (cl-opencv:destroy-window window-name)
    (cl-opencv:release-capture capture)))
	 
	