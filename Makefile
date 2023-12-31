CSC := $(shell which csc)
CSC_FLAGS = -O3 -specialize -strict-types
 
PROGRAM := advent

BUILD_DIR := build
SRC_DIR := src

SRCS := $(shell find $(SRC_DIR) -name *.scm -exec basename {} \;)
OBJS := $(patsubst %.scm,$(BUILD_DIR)/%.o,$(SRCS))
 
$(PROGRAM): $(OBJS)
	@echo "LD     $(PROGRAM)"
	@$(CSC) $(CSC_FLAGS) $(OBJS) -o $(BUILD_DIR)/$(PROGRAM)
	@echo ""

$(BUILD_DIR)/%.o : src/%.scm $(BUILD_DIR)
	@echo "CSC    $<"
	@$(CSC) $(CSC_FLAGS) $< -c -o $@

$(BUILD_DIR):
	@echo "MK     $(BUILD_DIR)"
	@mkdir -p $(BUILD_DIR)

clean:
	@echo "RM     $(BUILD_DIR)"
	@rm -rf $(BUILD_DIR)

cleandir distclean realclean: clean

run:	$(PROGRAM)
	./$(BUILD_DIR)/$(PROGRAM)
