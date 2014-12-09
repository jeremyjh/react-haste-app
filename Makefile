all: make_build static build/todomvc.js todomvc

build/todomvc.js: src/Main.hs src/Client.hs src/Types.hs
	hastec --debug -isrc --with-js=lib/stubs.js src/Main.hs -o $@

todomvc: src/Main.hs src/Client.hs src/Types.hs src/Server.hs
	ghc --make -isrc src/Main.hs -o todomvc

build/: 
	mkdir -p $@

make_build: build/ 

static: build/base.css build/bg.png build/todomvc.html build/react-with-addons.js

build/react-with-addons.js: lib/react-with-addons.js
	cp -f $< $@
build/base.css: static/base.css 
	cp -f $< $@
build/bg.png: static/bg.png
	cp -f $< $@
build/todomvc.html: static/todomvc.html
	cp -f $< $@

clean:
	-rm -rf build
	-rm -rf main
	-rm todomvc
	-rm src/*.hi
	-rm src/*.o

distclean: clean
	-rm todomvc
	-rm todomvc.js
