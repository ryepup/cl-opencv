#!/bin/sh
sbcl --noinform --eval "(asdf:operate 'asdf:load-op :cl-opencv-test)" \
    --eval "(cl-opencv-test:camera-frame-diff)" \
    --eval "(sb-ext:quit)"
exit 0
