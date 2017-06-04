CC = "/cygdrive/c/Program Files/Haskell Platform/8.0.2/bin/ghc"

.PHONY: clean

prog: main.hs
	$(CC) main.hs

clean:
	rm -f main.o main.exe
