SHELL := bash

.PHONY: 1
1:
	stack build kalyn
	time stack exec kalyn

.PHONY: 2
2:
	mkdir -p out-kalyn-rec
	ulimit -s unlimited && time out-kalyn/Main

.PHONY: 3
3:
	ulimit -s unlimited && time out-kalyn-rec/Main

.PHONY: clean
clean:
	@git clean -ffdX
