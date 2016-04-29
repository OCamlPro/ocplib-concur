-include autoconf/Makefile.config

TARGETS=$(foreach lib,$(CONCUR_TARGETS),ocplib-concur-$(lib))

all:
	ocp-build init
	ocp-build $(TARGETS)

tests:
	ocp-build

opam-deps:
	opam install lwt async ocplib-endian

clean:
	ocp-build clean

distclean:
	rm -rf _obuild
	rm -f autoconf/config.ocpgen
	rm -f autoconf/Makefile.config

install:
	ocp-build install $(TARGETS)
