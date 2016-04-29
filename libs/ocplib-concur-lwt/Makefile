include ../../autoconf/Makefile.config

TARGETS=$(foreach lib,$(CONCUR_TARGETS),ocplib-concur-$(lib))

all:
	ocp-build

async:
	ocp-build ocplib-concur-async

lwt:
	ocp-build ocplib-concur-lwt

