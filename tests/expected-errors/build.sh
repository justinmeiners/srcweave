#!/bin/sh

echo "EXPECT missing title"

if ../srcweave-test --force-output --tangle ./ missing-block-name.lit
then
   exit 1
fi

echo "EXPECT circular dependency"

if ../srcweave-test --force-output --tangle ./ circular.lit
then
   exit 1
fi

echo "EXPECT warning"
../srcweave-test --force-output --weave ./ --tangle ./ unused-block.lit
