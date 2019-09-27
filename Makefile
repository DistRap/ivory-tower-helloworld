#include ../stack.mk
OSNAME :=$(shell grep -o '^NAME=.*$$' /etc/os-release | cut -d'=' -f2 )
ifeq ($(OSNAME),NixOS)
       STACK=stack --nix
else
       STACK=stack
endif

TARGET ?= /dev/ttyACM0
IVORYFLAGS ?= --const-fold --verbose
TESTS      := \
	cansendrecv-test \
	can2uart-test \
	iot01a-test \
	uart-test \
	simpleblink-test \
	blink-test

AADL_TESTS := 
CLEANS     := $(foreach test,$(TESTS),$(test)-clean) \
	            $(foreach test,$(AADL_TESTS),$(test)_clean)
GDB := arm-none-eabi-gdb \
		--ex 'target extended-remote $(TARGET)' \
		--ex 'monitor connect_srst disable' \
		--ex 'monitor swdp_scan' \
		--ex 'set mem inaccessible-by-default off' \
		--ex 'attach 1'

.PHONY: test clean $(TESTS) $(AADL_TESTS) $(CLEANS)
test: $(TESTS) $(AADL_TESTS)
clean: $(CLEANS)

# FIXME: ideally we would build only target executable
# needs fixing in stack
# https://github.com/commercialhaskell/stack/issues/1406

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

define MK_AADL_TEST
$(1):
	$(STACK) build . --exec '$(1)_gen --src-dir=build_aadl/$(1) $(IVORYFLAGS)'
$(1)_clean:
	rm -rf build_aadl/$(1)
endef

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
$(foreach test,$(AADL_TESTS),$(eval $(call MK_AADL_TEST,$(test))))
