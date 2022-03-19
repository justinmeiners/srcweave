#!/bin/sh

BUNDLE=$1

#echo "#!$(command -v sbcl) --script"
echo "#!/usr/bin/env -S sbcl --script"
echo "(load \"$BUNDLE/bundle.lisp\")"
echo "(asdf:load-system \"srcweave\")"
echo "(srcweave:toplevel)"

