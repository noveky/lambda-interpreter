# Makefile for lambda interpreter
# Provides convenient targets for building, testing, and installation

.PHONY: all build clean install uninstall dev-install dev-uninstall run test help

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

# Standard install - uses dune install
install: build
	@echo "Installing via dune..."
	@dune install

# Standard uninstall - uses dune uninstall
uninstall:
	@echo "Uninstalling via dune..."
	@dune uninstall

# Development install - creates wrapper in ~/.local/bin
dev-install: build
	@echo "Installing lambda wrapper to $(WRAPPER_PATH)..."
	@mkdir -p $(INSTALL_DIR)
	@echo '#!/usr/bin/env bash' > $(WRAPPER_PATH)
	@echo '# Lambda interpreter wrapper (development installation)' >> $(WRAPPER_PATH)
	@echo 'exec dune exec --root="$(PROJECT_ROOT)" lambda --no-print-directory -- "$$@"' >> $(WRAPPER_PATH)
	@chmod +x $(WRAPPER_PATH)
	@echo "Development installation complete!"
	@echo ""
	@if echo ":$$PATH:" | grep -q ":$(INSTALL_DIR):"; then \
		echo "You can now run: lambda <file.lam>"; \
	else \
		echo "WARNING: $(INSTALL_DIR) is not in your PATH."; \
		echo "Add this to your ~/.bashrc or ~/.zshrc:"; \
		echo "    export PATH=\"\$$HOME/.local/bin:\$$PATH\""; \
		echo ""; \
	fi

# Development uninstall - removes the wrapper
dev-uninstall:
	@if [ -f $(WRAPPER_PATH) ]; then \
		echo "Removing $(WRAPPER_PATH)..."; \
		rm -f $(WRAPPER_PATH); \
		echo "Development uninstallation complete!"; \
	else \
		echo "Nothing to uninstall ($(WRAPPER_PATH) not found)"; \
	fi

# Run the interpreter (use: make run FILE=test.lam)
run: build
	@dune exec lambda --no-print-directory -- $(FILE)

# Display help
help:
	@echo "Lambda Interpreter Makefile"
	@echo ""
	@echo "Usage:"
	@echo "  make build           - Build the project"
	@echo "  make install         - Build and install via dune (copies to opam)"
	@echo "  make uninstall       - Uninstall via dune"
	@echo "  make dev-install     - Build and install 'lambda' wrapper to ~/.local/bin"
	@echo "  make dev-uninstall   - Remove the lambda wrapper from ~/.local/bin"
	@echo "  make clean           - Clean build artifacts"
	@echo "  make run FILE=<file> - Build and run a lambda file"
	@echo "  make help            - Show this help message"
	@echo ""
	@echo "Installation options:"
	@echo "  - 'make install' uses dune to copy binary to opam bin directory"
	@echo "  - 'make dev-install' creates a wrapper that calls 'dune exec' from project"
