# Makefile for lambda interpreter
# Provides convenient targets for building, testing, and installation

.PHONY: all build clean install uninstall run test help

# Project configuration
PROJECT_ROOT := $(shell pwd)
INSTALL_DIR := $(HOME)/.local/bin
WRAPPER_NAME := lambda
WRAPPER_PATH := $(INSTALL_DIR)/$(WRAPPER_NAME)

# Default target
all: build

# Build the project
build:
	@echo "Building lambda interpreter..."
	@dune build

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@dune clean

# Install - uses dune install by default, or dev mode with 'make install dev'
install:
	@if echo "$(word 2,$(MAKECMDGOALS))" | grep -q "dev"; then \
		echo "Installing lambda wrapper to $(WRAPPER_PATH)..."; \
		mkdir -p $(INSTALL_DIR); \
		echo '#!/usr/bin/env bash' > $(WRAPPER_PATH); \
		echo '# Lambda interpreter wrapper (development installation)' >> $(WRAPPER_PATH); \
		echo 'exec dune exec --root="$(PROJECT_ROOT)" lambda --no-print-directory -- "$$@"' >> $(WRAPPER_PATH); \
		chmod +x $(WRAPPER_PATH); \
		echo "Development installation complete!"; \
		echo ""; \
		if echo ":$$PATH:" | grep -q ":$(INSTALL_DIR):"; then \
			echo "You can now run: lambda <file.lam>"; \
		else \
			echo "WARNING: $(INSTALL_DIR) is not in your PATH."; \
			echo "Add this to your ~/.bashrc or ~/.zshrc:"; \
			echo "    export PATH=\"\$$HOME/.local/bin:\$$PATH\""; \
			echo ""; \
		fi \
	else \
		echo "Installing via dune..."; \
		dune build && dune install; \
	fi

# Uninstall - uses dune uninstall by default, or dev mode with 'make uninstall dev'
uninstall:
	@if echo "$(word 2,$(MAKECMDGOALS))" | grep -q "dev"; then \
		if [ -f $(WRAPPER_PATH) ]; then \
			echo "Removing $(WRAPPER_PATH)..."; \
			rm -f $(WRAPPER_PATH); \
			echo "Development uninstallation complete!"; \
		else \
			echo "Nothing to uninstall ($(WRAPPER_PATH) not found)"; \
		fi \
	else \
		echo "Uninstalling via dune..."; \
		dune uninstall; \
	fi

# Run the interpreter (use: make run test.lam)
run:
	@dune exec lambda --no-print-directory -- $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))

# Run a test file (use: make test [name] [args...])
# If no filename given, defaults to test.lam
test:
	@if [ -z "$(word 2,$(MAKECMDGOALS))" ]; then \
		cd test && dune exec lambda --no-print-directory -- test.lam; \
	else \
		cd test && dune exec lambda --no-print-directory -- $(word 2,$(MAKECMDGOALS)).lam $(wordlist 3,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS)); \
	fi

# Allow any target to be passed as argument
%:
	@

# Display help
help:
	@echo "Lambda Interpreter Makefile"
	@echo ""
	@echo "Usage:"
	@echo "  make build           - Build the project"
	@echo "  make install         - Build and install via dune (copies to opam)"
	@echo "  make install dev     - Build and install 'lambda' wrapper to ~/.local/bin"
	@echo "  make uninstall       - Uninstall via dune"
	@echo "  make uninstall dev   - Remove the lambda wrapper from ~/.local/bin"
	@echo "  make clean           - Clean build artifacts"
	@echo "  make run <file>      - Build and run a lambda file"
	@echo "  make test <name>     - Build and run test/<name>.lam"
	@echo "  make help            - Show this help message"
	@echo ""
	@echo "Installation options:"
	@echo "  - 'make install' uses dune to copy binary to opam bin directory"
	@echo "  - 'make install dev' creates a wrapper that calls 'dune exec' from project"
