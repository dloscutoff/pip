# Pip

Pip is an interpreted, imperative code-golf language. See the [GitHub wiki](https://github.com/dloscutoff/pip/wiki) for an introduction to the language, or the [documentation site](https://dloscutoff.github.io/pip/) for a language reference.

### Usage

The quickest way to get started using Pip is at [Try It Online!](https://tio.run/##K8gs@P8/3Sv7////Hqk5Ofn/w/OLclIUAQ) (Thanks, [Dennis](http://codegolf.stackexchange.com/users/12012/dennis)!)

Pip is implemented in Python3. The main interpreter is the pip.py file. It should run on most systems with Py3 installed simply by invoking `pip.py` in the directory where you put it (for \*nix systems, use `./pip.py`). You may also wish to modify the `PATH` environment variable to include the path to Pip, so that you can invoke it from anywhere. Typical invocation patterns:

`pip.py [flags] path/to/codefile.pip [args]`

`pip.py [flags] -e 'code' [args]`

`pip.py` (interactive mode)

Execute `pip.py --help` for more detailed information.

### Why Pip?

Pip's main reason for existence is to be a golfing language that 1) is imperative, and 2) uses infix operators. I do enjoy the challenge of stack-based programming from time to time, but I find the imperative paradigm much easier to think in, and therefore better. In [a survey of Code Golf StackExchange submissions](https://codegolf.meta.stackexchange.com/a/8891/16766), Pip scored slightly better than GolfScript and CJam, but not quite as good as Pyth.

### What does the name refer to?

[This fellow](http://en.wikipedia.org/wiki/Pip_(Great_Expectations)), of course.

Actually, the name "Pip" originated as a [recursive acronym](http://en.wikipedia.org/wiki/Recursive_acronym), though exactly what it stands for is open to debate. For some possibilities, see [The Tao of Pip](https://github.com/dloscutoff/pip/blob/master/Tao%20of%20Pip.txt). The name was also chosen for its connotations of smallness.

Pip is **not** to be confused with [pip](http://en.wikipedia.org/wiki/Pip_(package_manager)).
