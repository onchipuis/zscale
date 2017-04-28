#=======================================================================
# Makefile for Verilog simulation 
#-----------------------------------------------------------------------
# Ckristian Duran (ckristian.duran@correo.uis.edu.co)
#
# This makefile will build a rtl simulator and run various tests to
# verify proper functionality. (maybe)
#
# NOTE: for fast testing, use (with sbt):
# run-main zscale.Generator ./target/generated-src zscale ExampleTopZscale zscale DefaultConfig

SBT := sbt

# Variables
#TODO: change me: TestHarness
MODEL ?= ExampleTopZscale
PROJECT ?= zscale
CFG_PROJECT ?= $(PROJECT)
CONFIG ?= DefaultConfig
FIRRTL_JAR ?= rocket-chip/firrtl/utils/bin/firrtl.jar
FIRRTL ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -cp $(FIRRTL_JAR) firrtl.Driver

# Constants
long_name = $(PROJECT).$(CONFIG)
generated_dir = $(abspath ./target/generated-src)
base_dir = $(abspath .)
firrtl = $(generated_dir)/$(long_name).fir
verilog = $(generated_dir)/$(long_name).v

# Makefile rules
.SECONDARY: $(firrtl) $(verilog)

all default: verilog
verilog: $(verilog)

# Build firrtl.jar and put it where chisel3 can find it.
$(FIRRTL_JAR): $(shell find rocket-chip/firrtl/src/main/scala -iname "*.scala")
	$(MAKE) -C rocket-chip/firrtl SBT="$(SBT)" root_dir=rocket-chip/firrtl build-scala
	touch $(FIRRTL_JAR)
	mkdir -p ./lib
	cp -p $(FIRRTL_JAR) ./lib
# When chisel3 pr 448 is merged, the following extraneous copy may be removed.
	mkdir -p rocket-chip/chisel3/lib
	cp -p $(FIRRTL_JAR) rocket-chip/chisel3/lib
# Do the same for rocket-chip (for future testing)
	mkdir -p rocket-chip/lib
	cp -p $(FIRRTL_JAR) rocket-chip/lib

$(generated_dir)/%.fir $(generated_dir)/%.d: $(FIRRTL_JAR) $(chisel_srcs) $(bootrom_img)
	mkdir -p $(dir $@)
	cd $(base_dir) && $(SBT) "run-main $(PROJECT).Generator $(generated_dir) $(PROJECT) $(MODEL) $(CFG_PROJECT) $(CONFIG)"

$(generated_dir)/%.v $(generated_dir)/%.conf: $(generated_dir)/%.fir $(FIRRTL_JAR)
	mkdir -p $(dir $@)
	$(FIRRTL) -i $< -o $(generated_dir)/$*.v -X verilog --infer-rw $(MODEL) --repl-seq-mem -c:$(MODEL):-o:$(generated_dir)/$*.conf

clean:
	$(MAKE) -C rocket-chip/firrtl clean
	$(SBT) clean compiler-cache clean-files
	rm -rfv ./lib $(FIRRTL_JAR)

.PHONY: compile
