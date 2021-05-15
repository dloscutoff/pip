
# Getting started

There are a couple of ways to run Pip online, or you can clone the repository and run it locally.

## Online interpreters

The quickest way to get started using Pip is at [Try It Online!](https://tio.run/#pip) (Thanks, Dennis!) Note, however, that the version of Pip on TIO is [Pip Classic](https://github.com/dloscutoff/pip/releases/tag/v0.18), which doesn't have any of the updates and new features since 2018.

An up-to-date version of Pip is hosted at [Replit](https://replit.com/@dloscutoff/pip). Clicking the run button will drop you into an interactive mode session, which prompts for arguments and code and then executes the program. After execution completes, you can run another program using one of the command-line invocation styles below.

## Command-line

Pip is implemented in Python 3. The main interpreter is the `pip.py` file. It should run on most systems with Python 3 installed simply by invoking `pip.py` in the directory where you put it (for &ast;nix systems, use `./pip.py`).

You may also wish to modify the `PATH` environment variable to include the path to Pip, so that you can invoke it from anywhere. 

### Executing a pip program from file
`python3 pip.py [flags] path/to/codefile.pip [args]`

### Executing a pip program directly
`python3 pip.py [flags] -e 'code' [args]`

### Pip shell (interactive mode)
`python3 pip.py`

### Usage message
For more detailed information on how to invoke the interpreter:
`pip.py --help`
