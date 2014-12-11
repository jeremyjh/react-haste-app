all: static build/web/todomvc.js build/todomvc

deps:
	haste-inst install -j react-haskell lens-family
	cabal install -j react-haskell lens-family acid-state 

build/web/todomvc.js: src/*.hs
	hastec --debug -isrc --with-js=lib/stubs.js src/Main.hs -o $@

build/todomvc: src/*.hs
	ghc --make -isrc src/Main.hs -o build/todomvc

build/web/:
	mkdir -p $@

static: build/web/
	cp static/* build/web/
	cp lib/react-with-addons.js build/web/

clean:
	-rm -rf build
	-rm -rf main
	-rm -rf db
	-rm src/*.hi
	-rm src/*.o
