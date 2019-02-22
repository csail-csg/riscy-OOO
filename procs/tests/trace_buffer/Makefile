EXTPATH = -p +:$(RISCY_HOME)/procs/lib

LINK_JOBS ?= 8

all:
	mkdir -p buildDir
	bsc -u -sim \
		-bdir buildDir \
		-info-dir buildDir \
		-simdir buildDir \
		+RTS -K1000M -RTS \
		-show-schedule \
		-aggressive-conditions \
		-steps-warn-interval 1000000 \
		-show-range-conflict \
		-check-assert \
		-D BSIM \
		$(EXTPATH) \
		Tb.bsv
	grep -n "Blocking rules" \
		buildDir/mkTb.sched | \
		sed '/(none)/d'
	bsc -sim -e mkTb \
		+RTS -K1000M -RTS \
		-bdir buildDir \
		-info-dir buildDir \
		-simdir buildDir \
		-o simTb \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0 \
		-parallel-sim-link $(LINK_JOBS) \
		buildDir/*.ba

clean:
	rm -rf buildDir simTb* *.log

.PHONY: clean all simTb

.DEFAULT_GOAL := all
