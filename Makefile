# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -Iinclude -std=c99
LDFLAGS = 
DEBUGFLAGS = -g -O0
RELEASEFLAGS = -O2

# Directories
SRC_DIR = src
INCLUDE_DIR = include
BUILD_DIR = build
DEBUG_DIR = $(BUILD_DIR)/debug
RELEASE_DIR = $(BUILD_DIR)/release
TEST_DIR = tests

# Source files (find all .c files in src and subdirectories)
SRC_FILES = $(shell find $(SRC_DIR) -name '*.c')
OBJ_FILES_DEBUG = $(SRC_FILES:$(SRC_DIR)/%.c=$(DEBUG_DIR)/%.o)
OBJ_FILES_RELEASE = $(SRC_FILES:$(SRC_DIR)/%.c=$(RELEASE_DIR)/%.o)

# Target executable name
TARGET = rvm

# Default target
all: release

# Debug build
debug: CFLAGS += $(DEBUGFLAGS)
debug: $(DEBUG_DIR)/$(TARGET)

# Release build
release: CFLAGS += $(RELEASEFLAGS)
release: $(RELEASE_DIR)/$(TARGET)

# Link the debug binary
$(DEBUG_DIR)/$(TARGET): $(OBJ_FILES_DEBUG)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@
	@echo "Debug build complete: $@"

# Link the release binary
$(RELEASE_DIR)/$(TARGET): $(OBJ_FILES_RELEASE)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@
	@echo "Release build complete: $@"

# Compile source files for debug
$(DEBUG_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled (debug): $< -> $@"

# Compile source files for release
$(RELEASE_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled (release): $< -> $@"

# Clean up the build artifacts
clean:
	@rm -rf $(BUILD_DIR)
	@echo "Cleaned build directories."

# Run unit tests
test: debug
	@$(CC) $(CFLAGS) -I$(INCLUDE_DIR) -o $(TEST_DIR)/test_runner $(TEST_DIR)/test_main.c
	@$(TEST_DIR)/test_runner
	@echo "Tests executed."

# Phony targets
.PHONY: all debug release clean test
