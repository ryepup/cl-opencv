;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage #:cl-opencv-test
  (:use #:cl #:cl-opencv)
  (:export
   #:display
   #:show-camera
   #:show-camera-threshold
   #:camera-frame-diff
   #:camera-abs-diff
   #:camera-subtract))

