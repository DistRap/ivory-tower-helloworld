TARGET     ?= /dev/ttyACM0
IVORYFLAGS ?= --const-fold --verbose
#IVORYFLAGS ?=
APPS       :=
TESTS      := \
	bluepill-test \
	cansendrecv-test \
	can2uart-test \
	i2c-whoami-test \
	iot01a-test \
	monstick-test \
	monstick-rn2483-test \
	uart-test \
	simpleblink-test \
	ads-test \
	blink-test

CLEANS     := \
	$(foreach test,$(TESTS),$(test)-clean) \
	$(foreach app,$(APPS),$(app)-clean)

GDB := arm-none-eabi-gdb \
		--ex 'target extended-remote $(TARGET)' \
		--ex 'monitor connect_srst disable' \
		--ex 'monitor swdp_scan' \
		--ex 'set mem inaccessible-by-default off' \
		--ex 'attach 1'

.PHONY: test clean $(TESTS) $(CLEANS)
test: $(TESTS)
clean: $(CLEANS)

define MKTEST
$(1):
	cabal new-run $(1)-gen -- --src-dir=build/$(1) $(IVORYFLAGS)
	make -C build/$(1)
$(1)-clean:
	rm -rf dist-newstyle
	rm -rf build/$(1)
$(1)-gdb: $(1)
	$(GDB) build/$(1)/image
$(1)-gdbtui: $(1)
	$(GDB) -tui build/$(1)/image
$(1)-load: $(1)
	$(GDB) --ex 'load' build/$(1)/image
$(1)-run: $(1)
	$(GDB) --ex 'load' --ex 'run' build/$(1)/image
endef

$(foreach test,$(APPS),$(eval $(call MKTEST,$(test))))
$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
