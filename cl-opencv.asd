;;; -*- mode: lisp; indent-tabs: nil -*-

(asdf:defsystem #:cl-opencv
  :name "cl-opencv"
  :author "J. Bromley <jbromley@gmail.com>"
  :version "0.1"
  :description "OpenCV bindings for SBCL"
  :depends-on (:cffi)
  :serial t
  :components ((:file "package") 
	       (:file "highgui" :depends-on ("package"))))

