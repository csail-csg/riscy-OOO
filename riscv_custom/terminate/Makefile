PREFIX = riscv64-unknown-linux-gnu
TARGET = terminate

all:
	$(PREFIX)-g++ -I.. -static main.cpp -o $(TARGET)
	$(PREFIX)-objdump -d $(TARGET) > $(TARGET).dump
