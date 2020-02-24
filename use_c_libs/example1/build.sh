pushd  c
gcc --version
gcc -m64 -c foo.c -o foo.o
popd

pushd hs
ghc --version
ghc --make main.hs foo.hs ../c/foo.o
popd

