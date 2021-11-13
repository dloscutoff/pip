#!/usr/bin/env python3

import sys
import argparse
import pprint

import version
from scanning import scan, addSpaces
from parsing import parse
from ptypes import Scalar
from execution import ProgramState
from errors import FatalError


def pip(code=None, argv=None, interactive=True):
    if code is not None or argv is not None:
        interactive = False
    if interactive:
        print(f"=== Welcome to Pip, version {version.VERSION} ===")
        print("Enter command-line args, terminated by newline (-h for help):")
        argv = input()
    if argv is not None:
        # Artificial command-line input was provided
        if isinstance(argv, list):
            # Args are already in list form, just make sure each one
            # is a string
            argv = [str(arg) for arg in argv]
        else:
            # Parse the fake command-line input, simplistically accepting
            # single- and double-quoted strings (with no escapes or shell
            # expansion)
            argv_string = str(argv) + " "
            argv = []
            quote = None
            buffer = None
            for char in argv_string:
                if char in "'\"":
                    if quote is None:
                        # Open quote
                        quote = char
                        buffer = buffer or ""
                    elif quote == char:
                        # Close quote
                        quote = None
                    else:
                        # Already inside the other type of quote
                        buffer += char
                elif char == " " and quote is None:
                    if buffer is not None:
                        argv.append(buffer)
                    buffer = None
                else:
                    buffer = buffer or ""
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
                             help=("output list items on separate lines, "
                                   "concatenated"),
                             action="store_true")
    listFormats.add_argument("-n",
                             "--newline",
                             help="concatenate lists on newline",
                             action="store_true")
    listFormats.add_argument("-p",
                             "--repr",
                             help="print lists in repr form",
                             action="store_true")
    listFormats.add_argument("-P",
                             "--reprlines",
                             help=("output list items on separate lines, "
                                   "repr'd"),
                             action="store_true")
    argparser.add_argument("-r",
                           "--readlines",
                           help="read args from lines of stdin",
                           action="store_true")
    listFormats.add_argument("-s",
                             "--space",
                             help="concatenate lists on space",
                             action="store_true")
    listFormats.add_argument("-S",
                             "--spacelines",
                             help=("output list items on separate lines, "
                                   "space-concatenated"),
                             action="store_true")
    argparser.add_argument("-v",
                           "--verbose",
                           help="show extra messages",
                           action="store_true")
    argparser.add_argument("-V",
                           "--version",
                           help="display version info and quit",
                           action="store_true")
    argparser.add_argument("-w",
                           "--warnings",
                           help="show nonfatal warning messages",
                           action="store_true")
    argparser.add_argument("-x",
                           "--exec-args",
                           help=("treat each arg as Pip code (useful for "
                                 "args that need to be evaluated "
                                 "as expressions)"),
                           action="store_true")
    argparser.add_argument("args",
                           help="arguments to main function",
                           nargs="*")

    if argv is not None:
        # Parse options from artificial command-line input
        options = argparser.parse_args(argv)
    else:
        # Parse options from actual command-line input
        options = argparser.parse_args()
    #!print(options)

    if options.version:
        print(f"Pip {version.VERSION} (updated {version.COMMIT_DATE})")
        return
    if options.debug:
        options.warnings = options.verbose = options.repr = True
    listFormat = ("p" if options.repr else
                  "P" if options.reprlines else
                  "s" if options.space else
                  "S" if options.spacelines else
                  "n" if options.newline else
                  "l" if options.lines else
                  None)
    if not (code or options.execute or options.file or options.stdin):
        if interactive:
            options.stdin = True
            print("Enter your program, terminated by Ctrl-D or Ctrl-Z:")
        elif options.args:
            # Treat first non-option arg as name of code file
            options.file = options.args.pop(0)
        else:
            print(f"Type {sys.argv[0]} -h for usage information.")
            sys.exit(0)
    if code:
        # Code is passed into function
        program = code
    elif options.execute:
        # Code is given as command-line argument
        program = options.execute
    elif options.file:
        # Get code from specified file
        if interactive:
            print("Reading", options.file)
        try:
            with open(options.file) as f:
                program = f.read()
        except:
            print("Could not read from file", options.file, file=sys.stderr)
            sys.exit(1)
    elif options.stdin:
        # Get code from stdin, stopping at EOF
        program = ""
        try:
            while True:
                program += input() + "\n"
        except EOFError:
            pass
        if program:
            program = program[:-1]
    if options.verbose:
        charcount = len(program)
        bytecount = len(program.encode("utf-8"))
        print(f"{bytecount} bytes (UTF-8), {charcount} characters")
        print()
    try:
        tokens = scan(program)
    except FatalError:
        print("Fatal error while scanning, execution aborted.",
              file=sys.stderr)
        sys.exit(1)
    if options.verbose:
        print(addSpaces(tokens))
        print()
    try:
        parse_tree = parse(tokens)
    except FatalError:
        print("Fatal error while parsing, execution aborted.",
              file=sys.stderr)
        sys.exit(1)
    if options.verbose:
        pprint.pprint(parse_tree)
        print()
    state = ProgramState(listFormat, options.warnings)
    if options.readlines:
        raw_args = []
        try:
            while True:
                raw_args.append(input())
        except EOFError:
            pass
    else:
        raw_args = options.args
    if options.exec_args:
        # Treat each argument as a Pip statement/expression
        program_args = []
        for arg in raw_args:
            try:
                arg_tokens = scan(arg)
            except FatalError:
                print(f"Fatal error while scanning argument {arg!r}, "
                      "execution aborted.",
                      file = sys.stderr)
                sys.exit(1)
            try:
                arg_parse_tree = parse(arg_tokens)
            except FatalError:
                print(f"Fatal error while parsing argument {arg!r}, "
                      "execution aborted.",
                      file = sys.stderr)
                sys.exit(1)
            parsed_arg = arg_parse_tree[0]
            try:
                program_args.append(state.executeStatement(parsed_arg))
            except FatalError:
                print(f"Fatal error while evaluating argument {arg!r}, "
                      "execution aborted.",
                      file = sys.stderr)
                sys.exit(1)
            except KeyboardInterrupt:
                print("Program terminated by user while evaluating "
                      f"argument {arg!r}.",
                      file=sys.stderr)
                sys.exit(1)
            except RuntimeError as err:
                # Probably exceeded Python's max recursion depth
                print(f"Fatal error while evaluating argument {arg!r}:",
                      err,
                      file=sys.stderr)
                sys.exit(1)
    else:
        # Treat each argument as a Scalar
        program_args = [Scalar(arg) for arg in raw_args]
    if interactive:
        print("Executing...")
    try:
        state.executeProgram(parse_tree, program_args)
    except FatalError:
        print("Fatal error during execution, program terminated.",
              file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        print("Program terminated by user.", file=sys.stderr)
        sys.exit(1)
    except RuntimeError as err:
        # Probably exceeded Python's max recursion depth
        print("Fatal error:", err, file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        # No arguments given, just the name of the code file in argv
        pip(interactive=True)
    else:
        pip(interactive=False)


