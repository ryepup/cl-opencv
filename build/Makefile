# cl-opencv-glue 

include ../Makefile.opts
include ../Makefile.common

USR_OPENCV_LIBDIR=$(DESTDIR)/local/lib/
OPENCV_LIBDIR=$(DESTDIR)/lib/


all:

clean:
	$(RM) $(STUBS)

install_glue:
	install -m 0644 $(SHLIB_NAME).$(SHLIB_SUFFIX) $(USR_OPENCV_LIBDIR)

ifeq ($(wildcard $(OPENCV_LIBDIR)$(SHLIB_NAME).$(SHLIB_SUFFIX)),)
	@echo "Creating a soft link from $(SHLIB_NAME).$(SHLIB_SUFFIX) in $(USR_OPENCV_LIBDIR) to $(OPENCV_LIBDIR)"
	ln -s $(USR_OPENCV_LIBDIR)$(SHLIB_NAME).$(SHLIB_SUFFIX) $(OPENCV_LIBDIR)$(SHLIB_NAME).$(SHLIB_SUFFIX)
else
	@echo "Link exists from $(SHLIB_NAME).$(SHLIB_SUFFIX) in $(USR_OPENCV_LIBDIR) to $(OPENCV_LIBDIR)"
endif
