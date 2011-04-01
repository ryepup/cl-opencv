;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :cl-opencv
  (:use #:cl)
  (:export 
   ;; core - basic structures
   #:cv-point
   #:make-cv-point
   #:cv-point-p
   #:copy-cv-point
   #:cv-point-x
   #:cv-point-y

   #:size
   #:make-size
   #:size-p
   #:copy-size
   #:size-width
   #:size-height

   #:cv-rect
   #:make-cv-rect
   #:cv-rect-p
   #:copy-cv-rect
   #:cv-rect-x
   #:cv-rect-y
   #:cv-rect-width
   #:cv-rect-height

   #:make-cv-scalar
   #:cv-matrix

   #:ipl-image
   #:+ipl-depth-1u+
   #:+ipl-depth-8u+
   #:+ipl-depth-16u+
   #:+ipl-depth-32f+
   #:+ipl-depth-64f+
   #:+ipl-depth-8s+
   #:+ipl-depth-16s+
   #:+ipl-depth-32s+

   #:cv-array

   ;; core - operations on arrays
   #:abs-diff
   #:abs-diff-scalar
   #:add-weighted
   #:copy
   #:create-image
   #:get-size
   #:release-image
   #:reset-image-roi
   #:set-image-roi
   #:subtract
   #:subtract-scalar
   #:sub-r-scalar

   ;; imgproc - image processing -  miscellaneous image transformations
   #:+adaptive-thresh-mean-c+
   #:+adaptive-thresh-gaussian-c+
   #:adaptive-threshold
   #:+thresh-binary+
   #:+thresh-binary-inv+
   #:+thresh-trunc+
   #:+thresh-tozero+
   #:+thresh-tozero-inv+
   #:threshold

   ;; imgproc - image processing - image filtering
   #:ipl-conv-kernel
   #:+ipl-border-constant+
   #:+ipl-border-replicate+
   #:copy-make-border
   #:+cv-shape-rect+
   #:+cv-shape-cross+
   #:+cv-shape-ellipse+
   #:+cv-shape-custom+
   #:create-structuring-element-ex
   #:dilate
   #:erode
   ; #:filter-2d
   #:laplace
   #:+gaussian-5x5+
   #:pyr-down
   #:release-structuring-element

   ;; highgui - user interface
   #:cv-capture
   #:cv-video-writer

   #:+cvtimg-flip+
   #:+cvtimage-swap-rb+
   #:convert-image
   ; #:create-trackbar
   #:destroy-all-windows
   #:destroy-window
   ; #:get-trackbar-pos
   ; #:init-system
   #:move-window
   #:+window-normal+
   #:+window-autosize+
   #:named-window
   #:resize-window
   ; #:set-mouse-callback
   ; #:set-trackbar-pos
   #:show-image
   #:wait-key

   ;; highgui - reading and writing images and video
   #:+load-image-unchanged+
   #:+load-image-grayscale+
   #:+load-image-color+
   #:+load-image-anydepth+
   #:+load-image-anycolor+
   #:load-image
   #:load-image-m
   #:save-image
   #:create-camera-capture
   #:create-file-capture
   #:with-capture
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
   #:get-capture-property
   #:grab-frame
   #:query-frame
   #:release-capture
   #:retrieve-frame
   #:set-capture-property
   ; #:fourcc
   ; #:create-video-writer
   ; #:release-video-writer
   ; #:write-frame

))