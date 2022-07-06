# Targets
TARGETS := libtomsfastmath.a

# Header files/directories this library provides
HDRFILES := $(wildcard ${SOURCE_DIR}/src/headers/*)

CFILES := \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/addsub/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/bin/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/bit/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/divide/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/exptmod/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/generators/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/misc/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/mont/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/mul/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/numtheory/*.c)) \
  $(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/sqr/*.c)) \

include $(SEL4_COMMON)/common.mk
