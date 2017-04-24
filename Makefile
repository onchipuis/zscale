SBT := sbt
FIRRTL_JAR ?= rocket-chip/firrtl/utils/bin/firrtl.jar
FIRRTL ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -cp $(FIRRTL_JAR) firrtl.Driver

all default: compile

compile: $(FIRRTL_JAR)
	$(SBT) compile

# Build firrtl.jar and put it where chisel3 can find it.
$(FIRRTL_JAR): $(shell find rocket-chip/firrtl/src/main/scala -iname "*.scala")
	$(MAKE) -C rocket-chip/firrtl SBT="$(SBT)" root_dir=rocket-chip/firrtl build-scala
	touch $(FIRRTL_JAR)
	mkdir -p ./lib
	cp -p $(FIRRTL_JAR) ./lib
# When chisel3 pr 448 is merged, the following extraneous copy may be removed.
	mkdir -p rocket-chip/chisel3/lib
	cp -p $(FIRRTL_JAR) rocket-chip/chisel3/lib

clean:
	#make -C rocket-chip/chisel3 clean
	$(MAKE) -C rocket-chip/firrtl clean
	$(SBT) clean compiler-cache clean-files
	rm -rfv ./lib $(FIRRTL_JAR)

.PHONY: compile
