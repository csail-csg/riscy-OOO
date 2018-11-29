EXTPATH = -p +:$(RISCY_HOME)/procs/lib \
		  -p +:$(RISCY_HOME)/procs/RV64G_OOO \
		  -p +:$(RISCY_HOME)/coherence/src

MACROS = -D SECURITY \
		 -D CACHE_LARGE \
		 -D CORE_MEDIUM \
		 -D NUM_CORES=1

all:
	mkdir -p buildDir
	bsc -u -sim \
		-bdir buildDir \
		-info-dir buildDir \
		-simdir buildDir \
		-vdir buildDir \
		-aggressive-conditions \
		-check-assert \
		-show-range-conflict \
		-show-schedule \
		$(EXTPATH) $(MACROS) Tb.bsv
	bsc -sim -e mkTb \
		-bdir buildDir \
		-info-dir buildDir \
		-simdir buildDir \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0 \
		-o simTb buildDir/*.ba

clean:
	rm -rf buildDir sim*

.PHONY: clean all

.DEFAULT_GOAL := all
