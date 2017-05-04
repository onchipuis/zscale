# Parameters
XLEN ?= 64
TEST_MINI ?= add

# Constants
TEST_OBJS = $(addsuffix .o,$(basename $(wildcard tests/rv$(XLEN)/*.S)))
FIRMWARE_OBJS = firmware/start.o firmware/print.o firmware/sieve.o firmware/multest.o firmware/stats.o
GCC_WARNS  = -Werror -Wall -Wextra -Wshadow -Wundef -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings
GCC_WARNS += -Wredundant-decls -Wstrict-prototypes -Wmissing-prototypes -pedantic # -Wconversion
TOOLCHAIN_PREFIX = riscv$(XLEN)-unknown-elf-

firmware_mini.hex: firmware/firmware_mini.bin firmware/makehex.py
	python3 firmware/makehex.py $< 16384 > $@

firmware/firmware_mini.bin: firmware/firmware_mini.elf
	$(TOOLCHAIN_PREFIX)objcopy -O binary $< $@
	chmod -x $@

firmware/firmware_mini.elf: firmware/start_mini.o tests/rv$(XLEN)/$(TEST_MINI).o firmware/sections.lds
	$(TOOLCHAIN_PREFIX)gcc -Os -march=rv$(XLEN)im -ffreestanding -nostdlib -o $@ \
		-Wl,-Bstatic,-T,firmware/sections.lds,-Map,firmware/firmware_mini.map,--strip-debug \
		firmware/start_mini.o tests/rv$(XLEN)/$(TEST_MINI).o -lgcc
	chmod -x $@
	
firmware/start_mini.o: firmware/start_mini.S
	$(TOOLCHAIN_PREFIX)gcc -c -march=rv$(XLEN)im -o $@ $<

firmware.hex: firmware/firmware.bin firmware/makehex.py
	python3 firmware/makehex.py $< 16384 > $@

firmware/firmware.bin: firmware/firmware.elf
	$(TOOLCHAIN_PREFIX)objcopy -O binary $< $@
	chmod -x $@

firmware/firmware.elf: $(FIRMWARE_OBJS) $(TEST_OBJS) firmware/sections.lds
	$(TOOLCHAIN_PREFIX)gcc -Os -march=rv$(XLEN)im -ffreestanding -nostdlib -o $@ \
		-Wl,-Bstatic,-T,firmware/sections.lds,-Map,firmware/firmware.map,--strip-debug \
		$(FIRMWARE_OBJS) $(TEST_OBJS) -lgcc
	chmod -x $@

firmware/start.o: firmware/start.S
	$(TOOLCHAIN_PREFIX)gcc -c -march=rv$(XLEN)im -o $@ $< -DXLEN=$(XLEN)

firmware/%.o: firmware/%.c
	$(TOOLCHAIN_PREFIX)gcc -c -march=rv$(XLEN)im -Os --std=c99 $(GCC_WARNS) -ffreestanding -nostdlib -o $@ $<

tests/rv$(XLEN)/%.o: tests/rv$(XLEN)/%.S tests/macros/scalar/riscv_test.h tests/macros/scalar/test_macros.h
	$(TOOLCHAIN_PREFIX)gcc -c -march=rv$(XLEN)im -o $@ -DTEST_FUNC_NAME=$(notdir $(basename $<)) \
		-DTEST_FUNC_TXT='"$(notdir $(basename $<))"' -DTEST_FUNC_RET=$(notdir $(basename $<))_ret -I./tests/macros/scalar $<

seecode: firmware/firmware.elf
	$(TOOLCHAIN_PREFIX)objdump -d -M numeric,no-aliases firmware/firmware.elf | less

clean-firmware:
	rm -vrf $(FIRMWARE_OBJS) $(TEST_OBJS) \
		firmware/firmware.elf firmware/firmware.bin firmware/firmware.hex firmware/firmware.map \
		firmware/firmware_mini.elf firmware/firmware_mini.bin firmware/firmware_mini.hex firmware/firmware_mini.map firmware/*.o *.hex

