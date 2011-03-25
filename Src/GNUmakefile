# ============================================================================
# This is GNU makefile
#
# type:
#   'make'          to make your project(s)
# ============================================================================
# Default rule: make *.tx3 into *App.ob2 and *.prj into *.exe

ROOT := ..
# space-separated list of .prj files to compile into *.exe
PROJECT := SmallExcel.prj

PRJ_EXE := $(patsubst %.prj,%.exe,$(PROJECT))

default: $(PRJ_EXE)

# ============================================================================
# Include common hidden rules and some variables (see Rules.make)

include Rules.make
