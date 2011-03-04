# Makefile for cl-opencv glue code 

DESTDIR=/usr

all: Makefile.opts
	${MAKE} -C glue

clean: Makefile.opts
	${MAKE} -C glue clean
	${MAKE} -C build clean
	$(RM) Makefile.opts

Makefile.opts:
	(sh select_platform.sh)

install_glue:
	${MAKE} -C build install_glue DESTDIR=$(DESTDIR)

install: install_glue
