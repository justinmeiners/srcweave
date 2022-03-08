#!/bin/sh

PREFIX=$1
FILE=$PREFIX/bin/srcweave

echo "#!$(command -v sbcl) --script" > $FILE
echo "(load \"$PREFIX/lib/srcweave/bundle.lisp\")" >> $FILE
echo "(asdf:load-system \"srcweave\")" >> $FILE
echo "(srcweave:toplevel)" >> $FILE

