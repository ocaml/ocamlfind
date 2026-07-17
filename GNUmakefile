include Makefile.config

ifeq "$(IS_RELOCATABLE)" "1"
  HASH := \#
  SRCDIR_ENCODED = $(subst =,%+,$(subst :,%.,$(subst %,%$(HASH),$(SRCDIR_ABS))))
  SRCDIR_ABS_REAL := $(shell realpath $(SRCDIR_ABS) 2>/dev/null)
  SRCDIR_REAL_ENCODED = \
    $(subst =,%+,$(subst :,%.,$(subst %,%$(HASH),$(SRCDIR_ABS_REAL))))
  BUILD_PATH_PREFIX_MAP ?=
  export BUILD_PATH_PREFIX_MAP := \
    $(BUILD_PATH_PREFIX_MAP)$\
      :.=$(SRCDIR_ENCODED)$\
      $(if $(SRCDIR_REAL_ENCODED),:.=$(SRCDIR_REAL_ENCODED))
endif

include Makefile
