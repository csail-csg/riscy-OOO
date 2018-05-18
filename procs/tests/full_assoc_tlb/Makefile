EXTPATH = -p +:$(RISCY_HOME)/procs/lib

compile.Unit: compile.%:
	mkdir -p build$*Dir
	bsc -u -sim \
		-bdir build$*Dir \
		-info-dir build$*Dir \
		-simdir build$*Dir \
		-vdir build$*Dir \
		-aggressive-conditions \
		-check-assert \
		-show-range-conflict \
		-show-schedule \
		$(EXTPATH) \
		-D BSIM \
		Tb$*.bsv

tb.Unit: compile.Unit
	bsc -sim -e mkTbUnit \
		-bdir buildUnitDir \
		-info-dir buildUnitDir \
		-simdir buildUnitDir \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0 \
		-o simTbUnit buildUnitDir/*.ba

all: tb.Unit

clean:
	rm -rf build*Dir sim* *.txt

.PHONY: clean all compile.Unit tb.Unit

.DEFAULT_GOAL := all
