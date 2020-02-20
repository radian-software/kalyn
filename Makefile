kalyn: Main.hs
	ghc Main.hs -o kalyn

.PHONY: clean
clean:
	git clean -ffdX
