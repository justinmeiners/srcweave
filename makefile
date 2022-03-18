DIR=build/local-projects/srcweave
PREFIX=/usr/local
BUNDLE=${PREFIX}/lib/bundle.lisp

.PHONY: build clean install

build:
	sbcl --noinform --non-interactive --eval '(ql:bundle-systems (list "alexandria" "cl-ppcre" "unix-opts" "uiop") :to "build/")'
	mkdir -p ${DIR}
	cp -r bin ${DIR}/bin
	cp *.lisp ${DIR}
	cp *.asd ${DIR}
	echo "Bundle is built"

clean:
	rm -rf build

install:
	mkdir -p ${PREFIX}/lib
	./gen-script.sh ${PREFIX}
	rsync -a --delete -r build/ "${PREFIX}/lib/srcweave/"
	rsync -a bin/srcweave "${PREFIX}/bin/"
	rsync -a bin/srcweave-format "${PREFIX}/bin/"
	rsync -a bin/srcweave-format-init "${PREFIX}/bin/"
