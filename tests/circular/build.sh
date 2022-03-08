#!/bin/sh
../srcweave-test --force-output --tangle ./ --weave ./ circular.lit || exit 0
