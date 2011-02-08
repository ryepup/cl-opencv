;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :cl-opencv
  (:use #:cl #:cffi)
  (:export 
   #:ipl-image
   #:capture
   #:+window-normal+
   #:+window-autosize+
   #:named-window
   #:destroy-window
   #:destroy-all-windows

   #:+load-image-unchanged+
   #:+load-image-grayscale+
   #:+load-image-color+
   #:+load-image-anydepth+
   #:+load-image-anycolor+
   #:load-image

   #:release-image
   #:show-image
   #:wait-key
   #:create-camera-capture
   #:grab-frame
   #:retrieve-frame
   #:query-frame
   #:release-capture

   #:+cap-prop-pos-msec+
   #:+cap-prop-pos-frames+
   #:+cap-prop-pos-avi-ratio+
   #:+cap-prop-frame-width+
   #:+cap-prop-frame-height+
   #:+cap-prop-fps+
   #:+cap-prop-fourcc+
   #:+cap-prop-frame-count+
   #:+cap-prop-format+
   #:+cap-prop-mode+
   #:+cap-prop-brightness+
   #:+cap-prop-contrast+
   #:+cap-prop-saturation+
   #:+cap-prop-hue+
   #:+cap-prop-gain+
   #:+cap-prop-exposure+
   #:+cap-prop-convert-rgb+
   #:+cap-prop-white-balance+
   #:+cap-prop-rectification+
   #:set-capture-property
   #:get-capture-property))
