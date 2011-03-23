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
	-del /s /q A3\obj\* >NUL
	make default

graph:
	$(MAKE) graph -C A3
	$(MAKE) graph -C A3Edit
	$(MAKE) graph -C Src
	cp A3\graph.png .\A3.png
	cp A3Edit\graph.png .\A3Edit.png
	cp Src\graph.png .\Src.png
