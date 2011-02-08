#!/bin/sh
sbcl --noinform --eval "(asdf:operate 'asdf:load-op :cl-opencv-test)" \
    --eval "(cl-opencv-test:show-camera)" --eval "(sb-ext:quit)"
exit 0
