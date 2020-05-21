.PHONY: 1
1:
	stack build kalyn
	stack exec kalyn

.PHONY: 2
2:
	mkdir -p out-kalyn-rec
	ulimit -s unlimited && out-kalyn/Main

.PHONY: 3
3:
	ulimit -s unlimited && out-kalyn-rec/Main

.PHONY: clean
clean:
	@git clean -ffdX
