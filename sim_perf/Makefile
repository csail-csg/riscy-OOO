all:
	mkdir -p build
	g++ -O3 -std=c++11 \
		-I$(RISCY_TOOLS)/include \
		-I$(RISCY_HOME)/tools/riscv-isa-sim \
		-I$(RISCY_HOME)/tools/riscv-isa-sim/softfloat \
		-L$(RISCY_TOOLS)/lib -o build/sim $(wildcard src/*.cpp) -lpthread -lriscv
