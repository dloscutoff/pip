#!/usr/bin/python3

# Priorities TODO:
#  TZ (for transpo'z'e) operator
#  Figure out how _ expressions are going to work: currently leaning toward
#    generating them at runtime when expressions are evaluated, so that
#    2+t+_ turns into {12+a} (but note wrinkle: _+2+t turns into {a+2+10}
#    because it's (_+2)+t -> {a+2}+t and stuff isn't evaluated again once
#    it's inside the braces)
#  Special variables q and r
#  Fold meta-operator respects the associativity of the operator (R or L)
#  \ map meta-operator
#  Make all meta-operators orthogonal, i.e. combinable
#  Limited backslash-escapes in strings
#  Make M on strings produce string results?
#  Built-in functions that are implemented in Python code (z, for example)
#  Splat-calling of functions
#  Simple regex operations
#  Figure out how to get correct warning/error reporting in ptypes classes
#  Reconstitute code from parse tree and print that for debugging info?
#  -r option
#  More operators! Esp. random, string, and math operators needed

from scanning import scan, addSpaces
from parsing import parse
from execution import ProgramState
from errors import FatalError
import sys, argparse

VERSION = "0.15.04.26"

def pip(interactive=True):
    if interactive:
        sys.argv = sys.argv[:1]
        print("=== Welcome to Pip, version {} ===".format(VERSION))
        print("Enter command-line args, terminated by newline (-h for help):")
        args = input() + " "
        # Parse the fake command-line input, simplistically accepting single-
        # and double-quoted strings (with no escapes or shell expansion)
        quote = None
        buffer = ""
        for char in args:
            if char in "'\"":
                if quote is None:
                    quote = char
                elif quote == char:
                    quote = None
            elif char == " " and quote is None:
                if buffer != "":
                    sys.argv.append(buffer)
                buffer = ""
            else:
                buffer += char
    argparser = argparse.ArgumentParser()
    codeSources = argparser.add_mutually_exclusive_group()
    listFormats = argparser.add_mutually_exclusive_group()
    argparser.add_argument("-d",
                           "--debug",
                           help="equivalent to -pvw",
                           action="store_true")
    codeSources.add_argument("-e",
                             "--execute",
                             help="execute the given code")
    codeSources.add_argument("-f",
                             "--file",
                             help="execute code from the given file")
    codeSources.add_argument("-i",
                             "--stdin",
                             help="execute code read from stdin",
                             action="store_true")
    listFormats.add_argument("-l",
                             "--lines",
                             help="output list items space-concatenated on"
                                  + " separate lines",
                             action="store_true")
    listFormats.add_argument("-n",
                             "--newline",
                             help="concatenate lists on newline",
                             action="store_true")
    listFormats.add_argument("-p",
                             "--repr",
                             help="print lists in repr form",
                             action="store_true")
    listFormats.add_argument("-s",
                             "--space",
                             help="concatenate lists on space",
                             action="store_true")
    argparser.add_argument("-v",
                           "--verbose",
                           help="show extra messages",
                           action="store_true")
    argparser.add_argument("-w",
                           "--warnings",
                           help="show nonfatal warning messages",
                           action="store_true")
    argparser.add_argument("args",
                           help="arguments to main function",
                           nargs="*")
    options = argparser.parse_args()
    #!print(options)
    if options.debug:
        options.warnings = options.verbose = options.repr = True
    listFormat = ("p" if options.repr else
                  "s" if options.space else
                  "n" if options.newline else
                  "l" if options.lines else
                  None)
    if not (options.execute or options.file or options.stdin):
        if interactive:
            options.stdin = True
            print("Enter your program, terminated by Ctrl-D or Ctrl-Z:")
        elif options.args:
            # Treat first non-option arg as name of code file
            options.file = options.args.pop(0)
        else:
            print("Type {} -h for usage information.".format(sys.argv[0]))
            return
    if options.execute:
        # Code is given as command-line argument
        program = options.execute + "\n"
    elif options.file:
        # Get code from specified file
        if interactive:
            print("Reading", options.file)
        try:
            with open(options.file) as f:
                program = f.read() + "\n"
        except:
            print("Could not read from file", options.file, file=sys.stderr)
            return
    elif options.stdin:
        # Get code from stdin, stopping at EOF
        program = "\n"
        try:
            while True:
                program += input() + "\n"
        except EOFError:
            pass
    try:
        tkns = scan(program)
    except FatalError:
        print("Fatal error while scanning, execution aborted.", file=sys.stderr)
        return
    if options.verbose:
        print(addSpaces(tkns))
    try:
        tree = parse(tkns)
    except FatalError:
        print("Fatal error while parsing, execution aborted.", file=sys.stderr)
        return
    if options.verbose:
        print(tree)
    state = ProgramState(listFormat, options.warnings)
    if interactive:
        print("Executing...")
    try:
        state.executeProgram(tree, options.args)
    except FatalError:
        print("Fatal error during execution, program terminated.",
              file=sys.stderr)
    except KeyboardInterrupt:
        print("Program terminated by user.", file=sys.stderr)
    except RuntimeError as e:
        # Probably exceeded Python's max recursion depth
        print("Fatal error:", e, file=sys.stderr)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        # No arguments given, just the name of the code file in argv
        pip(interactive=True)
    else:
        pip(interactive=False)


