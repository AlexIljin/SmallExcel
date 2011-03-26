# ============================================================================
# This is GNU makefile
#
# type:
#   'make'          to make your project(s)
# ============================================================================

.PHONY: default test FORCE graph

default:
	$(MAKE) -C Src

FORCE:
	-del /s /q A3Lib\sym\* >NUL
	make default

graph:
	$(MAKE) graph -C Src
	cp Src\graph.png .\Src.png
