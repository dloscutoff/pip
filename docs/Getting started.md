
# Getting started

The quickest way to get started using Pip is at [Try It Online!](https://tio.run/#pip) (Thanks, Dennis!)

Pip is implemented in Python 3.
The main interpreter is the `pip.py` file. It should run on most systems with Python 3 installed simply by invoking pip.py in the directory where you put it (for \*nix systems, use ./pip.py).

You may also wish to modify the PATH environment variable to include the path to Pip, so that you can invoke it from anywhere. 

## Invoking the interpreter

### Executing a pip program from file
`python3 pip.py [flags] path/to/codefile.pip [args]`

### Executing a pip program directly
`python3 pip.py [flags] -e 'code' [args]`

### Pip shell (interactive mode)
`python3 pip.py`

### Usage message
For more detailed information on how to invoke the interpreter:
`pip.py --help`
