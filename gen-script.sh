#!/bin/sh

PREFIX=$1

#echo "#!$(command -v sbcl) --script"
echo "#!/usr/bin/env -S sbcl --script"
echo "(load \"$PREFIX/lib/srcweave/bundle.lisp\")"
echo "(asdf:load-system \"srcweave\")"
echo "(srcweave:toplevel)"

