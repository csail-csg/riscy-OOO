EXTPATH = -p +:./bsv \
		  -p +:$(RISCY_HOME)/procs/lib \
		  -p +:$(RISCY_HOME)/coherence/src
TARGETS = TSO.Unit TSO.Integrate WEAK.Unit WEAK.Integrate
BUILD_DIRS = buildTSODir buildWEAKDir
LINK_JOBS ?= 16

define tso_macro
$(patsubst %,-D TSO_MM,$(filter TSO.%,$(1)))
endef

define build_dir
build$(basename $(1))Dir
endef

define top_module
mkTb$(patsubst .%,%,$(suffix $(1)))
endef

define bsv_file
./bsv/Tb$(patsubst .%,%,$(suffix $(1))).bsv
endef

$(TARGETS): %:
	mkdir -p $(call build_dir,$@)
	bsc -u -sim \
		-bdir $(call build_dir,$@) \
		-info-dir $(call build_dir,$@) \
		-simdir $(call build_dir,$@) \
		+RTS -K1000M -RTS \
		-show-schedule \
		-aggressive-conditions \
		-steps-warn-interval 1000000 \
		-show-range-conflict \
		-check-assert \
		-D BSIM \
		$(EXTPATH) \
		$(call tso_macro,$@) \
		$(call bsv_file,$@)
	grep -n "Blocking rules" \
		$(call build_dir,$@)/$(call top_module,$@).sched | \
		sed '/(none)/d'
	bsc -sim -e $(call top_module,$@) \
		+RTS -K1000M -RTS \
		-bdir $(call build_dir,$@) \
		-info-dir $(call build_dir,$@) \
		-simdir $(call build_dir,$@) \
		-o sim.$@ \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0 \
		-parallel-sim-link $(LINK_JOBS) \
		$(call build_dir,$@)/*.ba

all: $(TARGETS)

clean:
	rm -rf build*Dir sim* *.log

.PHONY: clean all $(TARGETS)

.DEFAULT_GOAL := all
