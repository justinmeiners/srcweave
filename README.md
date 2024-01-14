# srcweave

`srcweave` is a [literate programming](https://en.wikipedia.org/wiki/Literate_programming) system for ANY programming language.
`srcweave` helps you write code to be read by humans, not machines.

**Features:**

- Compatible with all programming languages. Uses markdown for prose.
- Great support for working with multiple files (even books!). Code and prose can be moved between files seamlessly.
- Outputs clean HTML documentation that is easy to read and customize.
- An optional beautifier includes syntax highlighting (google-prettify) and typeset math (KaTeX).
- Focused UNIX philosphy design. It does one thing well and plays nice with other tools like `make` (preserves mod dates).

## .lit file examples

The best part about `.lit` is you learn it just by reading the code!
Here are a few basic examples to get started with:

- [Hello world](https://github.com/justinmeiners/srcweave/tree/master/tests/hello/hello.lit)
- [Basic features](https://github.com/justinmeiners/srcweave/tree/master/tests/basic/basic.lit)

For more in-depth examples, see my article:

- [Write your own Virtual Machine](https://github.com/justinmeiners/lc3-vm)

## Getting started

**Requirements**

- `sbcl` with [quicklisp](https://quicklisp.org) installed
- `markdown` (I recommend the `discount` implementation).
- Recommended: A POSIX system with `sh`, `curl` and related commands for `srcweave-html-styler`.

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

## Styling

Included in the installation is a tool called `srcweave-html-styler` which converts default HTML into beautiful documents.
To use just add the `--style srcweave-html-styler` to any weave command.
Fore example:

    srcweave --weave doc/ --styler srcweave-html-styler index.lit

Once for every project, you will also need to run:

    srcweave-html-styler-init docs/

This downloads all the necessary JavaScript and CSS dependencies.

**Math typesetting**

Include the `-m` flag on `srcweave-styler-init` to download KaTex.
See the examples for how to use `TeX` in .lit.

**Custom styles**

An easy way to customize the style is to edit the CSS file created by `srcweave-html-styler-init`.
If that's insufficient you are encouraged to create your own styler program.
Stylers are just programs that take raw HTML in `stdin` and format it to `stdout`.
You can start from scratch, or modify a copy of `srcweave-html-styler`.

## Comparison with Literate

srcweave is inspired by Zach Yedidia's [Literate](https://zyedidia.github.io/literate/).
I like his program, have used it for several years, and am grateful for his contribution.
However, I have since developed my own preferences and ideas for improvement:

- Written in Common Lisp instead of D.
  This makes it more stable and portable (works on BSD, etc).
- More modular UNIX design.
  srcweave completely delegates HTML formatting and libraries to a shell script.
  It provides high quality document output right out of the box, but is much easier to customize.
- Simpler handling of multiple files.
  The "books" feature in Literate is a little cumbersome.
  In srcweave you can have multiple chapters in a single file,
  or divide them each into their own file. There is no distinction.
- GPL license instead of MIT.
  srcweave is a program for end users.
  GPL should not restrict commercial use of the program while maximizing user freedom (contact me if you have an issue).

The [vim plugin](https://github.com/zyedidia/literate.vim) should be compatible.

**Known incompatibilities:**

You can migrate files from Literate with only minor changes.
Here are the important differences:

- In Literate produces a file for any block title with a file extension (eg. "out.txt").
  In srcweave, all file blocks must be prefixed with a path (eg. `/out.txt` to create `out.txt` in the working directory).
- Styling commands like `@add_css`, `@colorscheme` are ignored.
  All styling is done with a separate tool instead (see "Styling" section above).
- No support for `@book`. Just pass multiple `.lit` files to `srcweave` in the order you want.
- No support for `@change`. Adjusting `.lit` files should be done using your build process, such as with a shell script or makefile.
- `@title` only sets the page title, it does not create a heading.
- Prefer markdown headings `# heading 1`  and `## heading 2` instead of `@s`, etc.

## Acknowledgments

We are very grateful to all our contributors:

- [Ryan Pendleton](https://github.com/rpendleton) for designing the document styler.
- [Eric Ihli](https://github.com/eihli) for creating an [Emacs mode](https://github.com/eihli/lit-mode) and guiding new features.

## License

[GPL 2](LICENSE.txt)
