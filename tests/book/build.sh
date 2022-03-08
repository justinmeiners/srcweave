#!/bin/sh

# This is also a test of file modification dates.

BUILD="../srcweave-test --tangle ./ --weave ./ index.lit chapter1.lit chapter2.lit"

# touch source files to reset everything
touch *.lit
$BUILD
# don't modify anything, and rerun.
touch .timestamp
$BUILD

(find -name .timestamp  -newer main.cpp | grep -q .) || (echo "modified main.cpp"; exit 1)
(find -name .timestamp  -newer main.cpp | grep -q .) || (echo "modified main.cpp"; exit 1)


# Modify one source file.
touch chapter1.lit
touch .timestamp
$BUILD

(find -name main.cpp -newer .timestamp | grep -q .) || (echo "modified main.cpp"; exit 1)
(find -name .timestamp -newer cpp.sh | grep -q .) || (echo "modified index when it shouldn't"; exit 1)

rm .timestamp
