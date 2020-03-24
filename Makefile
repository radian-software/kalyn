.PHONY: build
build:
	@stack build

.PHONY: clean
clean:
	@git clean -ffdX
