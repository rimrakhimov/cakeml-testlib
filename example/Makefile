TEST_UNIT_LIST = ../src/Assert.sml ../src/Exception.sml ../src/Test.sml ../src/TextUITestRunner.sml \
			Dictionary.sml TestDictionary.sml

# Change this directory if necessary  -- or
# provide the directory for your machine on the make command-line, e.g.
# make -n   CAKE_DIR="/someOtherLocation/cake-x64-64"
CAKE_DIR = ~/cake-x64-64
CAKEC = $(CAKE_DIR)/cake
BASIS = $(CAKE_DIR)/basis_ffi.c

OS ?= $(shell uname)
ifeq ($(OS),Darwin)
	# These options avoid linker warnings on macOS
	LDFLAGS += -Wl,-no_pie
endif

DEBUG = true
ifeq ($(DEBUG), true)
	CFLAGS += -ggdb3
else
	CFLAGS = -DNDEBUG
endif

CC = gcc
# CFLAGS =


.PHONY: all
all: cakeMlTestUnit clean

cakeMlTestUnit: cakeMlTestUnitFull.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

cakeMlTestUnitFull.S: cakeMlTestUnitFull.sml
	$(CAKEC) < cakeMlTestUnitFull.sml > cakeMlTestUnitFull.S

cakeMlTestUnitFull.sml: $(TEST_UNIT_LIST)
	cat $^ > $@

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

.PHONY: clean
clean:
	rm -f cakeMlTestUnitFull.S cakeMlTestUnitFull.sml basis_ffi.o