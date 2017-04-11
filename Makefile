CASK ?= cask

all: test

test: unit

unit:
	$(CASK) exec ert-runner

.PHONY: all test unit
