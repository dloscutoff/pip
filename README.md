# Pip

Pip is an interpreted, imperative code-golf language. For more on the syntax and some example programs, see the [Documentation](https://github.com/dloscutoff/pip/tree/master/Documentation) folder.

### Usage

*You can now run Pip code online at http://pip.tryitonline.net/! Thanks, [Dennis](http://codegolf.stackexchange.com/users/12012/dennis)!*

Pip is implemented in Python3. The main interpreter is the pip.py file. It should run on most systems with Py3 installed simply by invoking `pip.py` in the directory where you put it (for *nix systems, use `./pip.py`). You may also wish to modify the `PATH` environment variable to include the path to Pip, so that you can invoke it from anywhere. Typical invocation patterns:

`pip.py [flags] path/to/codefile.pip [args]`

`pip.py [flags] -e 'code' [args]`

`pip.py` (interactive mode)

Execute `pip.py --help` for more detailed information.

### Why Pip?

Pip's main reason for existence is to be a golfing language that 1) is imperative, and 2) uses infix operators. I do enjoy the challenge of stack-based programming from time to time, but I find the imperative paradigm much easier to think in, and therefore better. In my very unscientific testing so far, Pip has attained golfing scores roughly similar to those of GolfScript.

### What does the name refer to?

[This fellow](http://en.wikipedia.org/wiki/Pip_(Great_Expectations)), of course.

Actually, the name "Pip" originated as a [recursive acronym](http://en.wikipedia.org/wiki/Recursive_acronym), though exactly what it stands for is open to debate. For some possibilities, see The Tao of Pip. The name was also chosen for its connotations of smallness.

Pip is **not** to be confused with [pip](http://en.wikipedia.org/wiki/Pip_(package_manager)).
