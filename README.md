# srcweave

`srcweave` is a [literate programming](https://en.wikipedia.org/wiki/Literate_programming) system for ANY programming language.
`srcweave` helps you write code to be read by humans, not machines.

**Features:**

- Use any programming language for source. Write prose in markdown.
- HTML documentation output with syntax highlighting (google-prettify) and math typesetting (KaTeX).
- `make` friendly. Preserves file modification dates.
- Cross-reference code across multiple files.
- Compact UNIX philosophy design. Customize style and format with a shell script.

## Getting started

**Requirements**

- POSIX compliant system with tsort and shell.
- `sbcl` with [quicklisp](https://quicklisp.org) installed
- markdown (I recommend the `discount` implementation).

**Install**

Clone the project and run the following:

    make
    make install

**Usage**

1. Create an `index.lit` file. For example:

        # Test
        
        Test console output:

        --- hello.js
        console.log("Hello, World!");
        ---

2. Run `srcweave --tangle . index.lit` to generate source files (`hello.js`).

3. Run `srcweave --weave doc/ index.lit` to generate documentation (`index.html`).

Both `--tangle` and `--weave` can be included in the same invocation.
Multiple lit files can be specified, and the order they are listed in will determine the order of chapters and sections across files.

**Formatting**

The default HTML output is plain and unformatted.
`srcweave` includes a UNIX style filter which provides nice output.
To use it, you must first run

    srcweave-format-init docs/

This downloads all the necessary JavaScript/CSS dependencies for the project
and only needs to be run once.

Then specify the formatter on the weave command:

    srcweave --weave doc/ --formatter srcweave-format index.lit

If you need to customize the HTML output beyond CSS,
you are encouraged to copy the `srcweave-format` shell script to make your own.

**Math typesetting**

Include the `-m` flag on `srcweave-format-init` to download KaTex.
See the examples for how to use `TeX` in .lit.

## .lit file examples

The best part about .lit is you learn it just by reading the code!
Here are a few basic examples to get started with:

- [Hello world](https://github.com/justinmeiners/srcweave/tree/master/tests/hello/hello.lit)
- [Basic features](https://github.com/justinmeiners/srcweave/tree/master/tests/basic/basic.lit)

For more in-depth examples, see my article:

- [Write your own Virtual Machine](https://github.com/justinmeiners/lc3-vm)

## Comparison with Literate

srcweave is inspired by Zach Yedidia's [Literate](https://zyedidia.github.io/literate/).
I like his system and have used it for several years, but I have developed my own preferences.

- Written in Common Lisp instead of D.
  This makes it more stable and portable (BSD, etc).
- More modular UNIX design. srcweave completely delegates HTML formatting and libraries
  to a shell script. It provides the same features by default, but is much easier to customize.
- Simpler handling of multifile systems. The "books" feature is a little cumbersome.
- GPL license instead of MIT. It's intended to be used as a program, so
  this should not restrict commercial use while maximizing user freedom (contact me if you have an issue).

The [vim plugin](https://github.com/zyedidia/literate.vim) should be compatible.

**Known incompatibilities:**

You can move files from Literate with only minor changes.
Here are the major differences:

- formatting commands like `@add_css`, `@colorscheme` are ignored.
  Use the shared `.css` or a custom format script.
- no support for books `@book`. Just pass multiple `.lit` files to the tool in the order you want.
- no support for `@change` commands. Use shell scripts in your build process.
- `@title` only sets the page title.
- Prefer markdown `# heading 1`  and `## heading 2` instead of `@s`, etc.

## Acknowledgments

Thanks to [Ryan Pendleton](https://github.com/rpendleton) for designing the document formatter.

## License

[GPL 2](LICENSE.txt)
