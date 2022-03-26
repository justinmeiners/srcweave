rm -f styles/main.css
../../bin/srcweave-format-init -m ./
../srcweave-test --force-output --formatter ../../bin/srcweave-format --tangle src --weave ./ format.lit
