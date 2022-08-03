DIR=build/local-projects/srcweave
PREFIX?=/usr/local
BUNDLE=${PREFIX}/lib/bundle.lisp

SRC_LISP=$(wildcard *.lisp)
SRC_ASD=$(wildcard *.asd)

.PHONY: all clean install

all: build bin/srcweave

build: $(SRC_LISP) $(SRC_ASD)
	sbcl --noinform --non-interactive --eval '(ql:bundle-systems (list "alexandria" "cl-ppcre" "unix-opts" "uiop") :to "build/")'
	mkdir -p ${DIR}
	cp *.lisp ${DIR}
	cp *.asd ${DIR}
	echo "Bundle is built"

bin/srcweave: gen-script.sh
	./gen-script.sh "${PREFIX}/lib/srcweave" > bin/srcweave
	chmod +x bin/srcweave

clean:
	rm -f bin/srcweave
	rm -rf build

install:
	rm -rf "${PREFIX}/lib/srcweave"
	cp -r build "${PREFIX}/lib/srcweave"
	install bin/srcweave "${PREFIX}/bin/"
	install bin/srcweave-format "${PREFIX}/bin/"
	install bin/srcweave-format-init "${PREFIX}/bin/"
