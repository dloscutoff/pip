# Pip

Pip is an interpreted, imperative code-golf language. See the [GitHub wiki](https://github.com/dloscutoff/pip/wiki) for an introduction to the language, or the [documentation site](https://dloscutoff.github.io/pipdoc/) for a language reference.

### Why Pip?

Unlike most golfing languages, but like many practical languages, Pip is an imperative language with infix operators. It also uses plain ASCII instead of a custom codepage. These features make it a great introduction to golflangs for users of imperative languages like Python, JavaScript, and Perl.

In [a survey of Code Golf StackExchange submissions](https://codegolf.meta.stackexchange.com/a/8891/16766), Pip scored slightly better than GolfScript and CJam, but not quite as good as Pyth.

### Usage

You can run Pip at the following online interpreters:

- [Attempt This Online](https://ato.pxeger.com/run?1=m724ILNgwYKlpSVpuhaL072yISyowOZoJY_UnJx8JR0FpfD8opwURaVYqBQA) typically supports the latest release of Pip (but possibly not the latest commit). *(Thanks to [pxeger](https://github.com/pxeger) for adding Pip support to ATO!)*
- [Try It Online](https://tio.run/##K8gs@P8/3Sv7////Hqk5Ofn/w/OLclIUAQ) supports [version 0.18](https://github.com/dloscutoff/pip/releases/tag/v0.18), aka [Pip Classic](https://dloscutoff.github.io/pipdoc/pip-classic), which is a few years out of date. *(Thanks to [Dennis](https://github.com/DennisMitchell) for adding Pip support to TIO!)*
- [Replit](https://replit.com/@dloscutoff/pip) hosts the latest commit of Pip, with the downside that it uses a command-line interface and doesn't allow permalinking. Clicking the run button will drop you into an interactive  session, which prompts for arguments and code and then executes the program. *(Thanks to [razetime](https://github.com/razetime) for the idea of hosting Pip on Replit!)*

You can also clone the Pip repository and run it from the command line. Pip is implemented in Python 3. The main interpreter is the `pip.py` file. It should run on most systems with Python 3 installed simply by invoking `pip.py` in the directory where you put it (for &ast;nix systems, use `./pip.py`). You may also wish to modify the `PATH` environment variable to include the path to Pip, so that you can invoke it from anywhere. Typical invocation patterns:

`pip.py [flags] path/to/codefile.pip [args]`

`pip.py [flags] -e 'code' [args]`

`pip.py` (interactive mode)

Execute `pip.py --help` for more detailed information.

### What does the name refer to?

[This fellow](http://en.wikipedia.org/wiki/Pip_(Great_Expectations)), of course.

Actually, the name "Pip" originated as a [recursive acronym](http://en.wikipedia.org/wiki/Recursive_acronym), though exactly what it stands for is open to debate. For some possibilities, see [The Tao of Pip](https://github.com/dloscutoff/pip/blob/master/Tao%20of%20Pip.txt). The name was also chosen for its connotations of smallness.

Pip is **not** to be confused with [pip](http://en.wikipedia.org/wiki/Pip_(package_manager)).
