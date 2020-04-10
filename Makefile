.PHONY: all
all:
	./kalyn

.PHONY: build
build:
	stack build

.PHONY: clean
clean:
	@git clean -ffdX
