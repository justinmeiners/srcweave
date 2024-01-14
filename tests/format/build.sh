# test: mix trailing slash and non-trailing slash paths
rm -f styles/main.css
../../bin/srcweave-html-styler-init -m ./
../srcweave-test --force-output --styler ../../bin/srcweave-html-styler --tangle src --weave ./ format.lit
