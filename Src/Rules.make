# ============================================================================
# This file is part of GNUmakefile
# Some hidden rules (template rules) and internal variables are declared here
# to help you make your Amadeus project(s)
#
# The XDS/BIN directory should be in the PATH environment variable.
#
# ============================================================================
# Rules for building the application

include $(ROOT)/A3/Rules.make

%.exe: %.prj *.ob2 $(A3obj_files)
	$(XC) =project $<
	-@echo off && mkdir obj 2> NUL
	-@echo off && mv --force *.obj *.sym *.res tmp.lnk obj 2> NUL
