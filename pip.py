#!/usr/bin/env python3

import sys
import argparse
import pprint

import version
from scanning import scan, addSpaces
from operators import opsByArity, ARITIES, ASSOCIATIVITIES
from parsing import parse, parseFullProgram
from ptypes import Scalar
from execution import ProgramState, DEFAULT_VARS
from errors import FatalError, BadSyntax, IncompleteSyntax


REPL_HELP_TEXT = """
Enter Pip statements at the prompt. Entering an expression displays
its value. Incomplete statements/expressions can be continued on
subsequent lines. $_, $__, and $___ store the values of the last
three expressions.

>> Fi,3Pi
0
1
2
>> 6*7
42
>> [$_
..  '!]
[42;"!"]

REPL commands (abbreviated versions like ;h or ;he work too):

;warnings   Toggle display of warning messages (can also be invoked
            as ;warnings off or ;warnings on)
;quit       Quit the REPL
;exit       Identical to ;quit
;help AB    Display information about AB
;help       Display this help message
"""


def main():
    """Run the interpreter as a main program."""
    try:
        if len(sys.argv) == 1:
            # No arguments given, just the name of the code file in argv
            pip(interactive=True)
        else:
            pip(interactive=False)
    except FatalError:
        # The pip() function already gave the appropriate error output
        sys.exit(1)


def run(code=None, argv=None):
    """Run specific code (or interactive with no args) and catch errors."""
    try:
        if code is None and argv is None:
            pip(interactive=True)
        else:
            if argv is None:
                # If code is given and args are not, run with empty arglist
                argv = []
            pip(code, argv, interactive=False)
    except FatalError:
        # The pip() function already gave the appropriate error output
        pass


def pip(code=None, argv=None, interactive=True):
    # If in interactive mode, get artificial command-line args from stdin
    if code is not None or argv is not None:
        interactive = False
    if interactive:
        print(f"=== Welcome to Pip, version {version.VERSION} ===")
        print("Enter command-line args, terminated by newline "
              "(-h for help, -R for repl):")
        argv = input()

    # Process command-line args into the options object
    options = getArgs(argv)
    listFormat = ("p" if options.repr else
                  "P" if options.reprlines else
                  "s" if options.space else
                  "S" if options.spacelines else
                  "n" if options.newline else
                  "l" if options.lines else
                  None)

    # Some options cause behavior other than running a full program
    if options.version:
        print(f"Pip {version.VERSION} (updated {version.COMMIT_DATE})")
        return
    if options.repl:
        repl(listFormat, options.warnings)
        return

    # Get program code
    if (code is None and options.execute is None and options.file is None
            and not options.stdin):
        if interactive:
            options.stdin = True
        elif options.args:
            # Treat first non-option arg as name of code file
            options.file = options.args.pop(0)
        else:
            print(f"Type {sys.argv[0]} -h for usage information.")
            return
    if code is not None:
        # Code is passed into function
        program = code
    else:
        # Load code from some source based on options
        program = getCode(options, interactive)
    if options.verbose:
        charcount = len(program)
        bytecount = len(program.encode("utf-8"))
        print(f"{bytecount} bytes (UTF-8), {charcount} characters")
        print()

    # Scan the program into tokens
    try:
        tokens = scan(program)
    except FatalError as err:
        print("Fatal error while scanning:", err, file=sys.stderr)
        print("Execution aborted.", file=sys.stderr)
        raise
    if options.verbose:
        print(addSpaces(tokens))
        print()

    # Parse the tokens into a parse tree
    try:
        output_specifiers, parse_tree = parseFullProgram(tokens)
    except FatalError as err:
        print("Fatal error while parsing:", err, file=sys.stderr)
        print("Execution aborted.", file=sys.stderr)
        raise
    if options.verbose:
        pprint.pprint(parse_tree)
        print()

    # Create the program state object
    if ";" in output_specifiers:
        # Semicolon at end of program suppresses autoprinting
        autoprint = False
    else:
        autoprint = True
    state = ProgramState(listFormat, options.warnings, autoprint)

    # If the readlines option was specified, read program args from stdin
    if options.readlines:
        raw_args = []
        try:
            while True:
                raw_args.append(input())
        except EOFError:
            pass
    else:
        raw_args = options.args

    # If the exec_args option was specified, evaluate the args
    if options.exec_args:
        # Treat each argument as a Pip statement/expression
        program_args = []
        for arg in raw_args:
            try:
                arg_tokens = scan(arg)
            except FatalError as err:
                print(f"Fatal error while scanning argument {arg!r}:",
                      err, file=sys.stderr)
                print("Execution aborted.", file=sys.stderr)
                raise
            try:
                arg_parse_tree = parse(arg_tokens)
            except FatalError as err:
                print(f"Fatal error while parsing argument {arg!r}:",
                      err, file=sys.stderr)
                print("Execution aborted.", file=sys.stderr)
                raise
            parsed_arg = arg_parse_tree[0]
            try:
                program_args.append(state.evaluate(parsed_arg))
            except (FatalError, RuntimeError) as err:
                # RuntimeError probably means we exceeded Python's
                # max recursion depth
                print(f"Fatal error while evaluating argument {arg!r}:",
                      err, file=sys.stderr)
                print("Execution aborted.", file=sys.stderr)
                raise FatalError(str(err))
            except KeyboardInterrupt:
                print("Program terminated by user while evaluating "
                      f"argument {arg!r}.",
                      file=sys.stderr)
                raise FatalError("Keyboard interrupt")
    else:
        # Treat each argument as a Scalar
        program_args = [Scalar(arg) for arg in raw_args]

    # Execute the program
    if interactive:
        print("Executing...")
    try:
        state.executeProgram(parse_tree, program_args)
    except (FatalError, RuntimeError) as err:
        # RuntimeError probably means we exceeded Python's max
        # recursion depth
        print("Fatal error during execution:", err, file=sys.stderr)
        print("Program terminated.", file=sys.stderr)
        raise FatalError(str(err))
    except KeyboardInterrupt:
        print("Program terminated by user.", file=sys.stderr)
        raise FatalError("Keyboard interrupt")


def getArgs(argv):
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
    codeSources.add_argument("-R",
                             "--repl",
                             help="run as a read-eval-print loop",
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
    if options.debug:
        options.warnings = options.verbose = options.repr = True
    #!print(options)
    return options


def getCode(options, interactive):
    if options.execute is not None:
        # Code is given as command-line argument
        code = options.execute
    elif options.file is not None:
        # Get code from specified file
        if interactive:
            print("Reading", options.file)
        try:
            with open(options.file) as f:
                code = f.read()
        except:
            print("Could not read from file", options.file, file=sys.stderr)
            raise FatalError("Could not read from code file")
    elif options.stdin:
        # Get code from stdin, stopping at EOF
        if interactive:
            print("Enter your program, terminated by Ctrl-D or Ctrl-Z:")
        code = ""
        try:
            while True:
                code += input() + "\n"
        except EOFError:
            pass
        if code:
            code = code[:-1]
    return code


def repl(list_format=None, warnings=False):
    """Run read-eval-print loop."""
    print(f"Pip {version.VERSION}")
    print("Type ;help for more information.")
    state = ProgramState(list_format, warnings)
    try:
        while True:
            code = input(">> ") + "\n"
            parse_tree = None
            try:
                while parse_tree is None:
                    try:
                        tokens = scan(code)
                        parse_tree = parse(tokens)
                    except IncompleteSyntax:
                        # Error while scanning or parsing due to an
                        # incomplete string/expression/command; read
                        # another line, add it to the code, and try again
                        code += input(".. ") + "\n"
            except BadSyntax as err:
                # Error while scanning or parsing that cannot be
                # resolved by adding more code to the end; give up
                # on this code and start over
                print("Syntax error:", err, file=sys.stderr)
                continue
            # Some Pip comments are used as repl commands
            if code.startswith(";") and code[1:].strip():
                repl_command, *repl_cmd_args = code[1:].split(maxsplit=1)
                repl_command = repl_command.lower()
                if repl_cmd_args:
                    repl_cmd_arg = repl_cmd_args[0].strip()
                else:
                    repl_cmd_arg = None
                if ("quit".startswith(repl_command)
                        or "exit".startswith(repl_command)
                        or repl_command == "x"):
                    # Exit the repl
                    break
                elif "help".startswith(repl_command):
                    # Show help text
                    showHelp(repl_cmd_arg)
                elif "warnings".startswith(repl_command):
                    # Turn warnings on/off
                    status_changed = False
                    if repl_cmd_arg:
                        # With an argument, set status
                        arg = repl_cmd_arg.lower()
                        if arg == "on":
                            state.err.warnings = True
                            status_changed = True
                        elif arg == "off":
                            state.err.warnings = False
                            status_changed = True
                    else:
                        # Otherwise, toggle status
                        state.err.warnings = not state.err.warnings
                        status_changed = True
                    if status_changed:
                        print("Warnings",
                              "on" if state.err.warnings else "off",
                              file=sys.stderr)
            try:
                for statement in parse_tree:
                    result = state.executeStatement(statement)
                    if result is not None:
                        state.PRINT(state.REPR(result))
                        state.updateHistoryVars(result)
            except (FatalError, RuntimeError) as err:
                # RuntimeError probably means we exceeded Python's
                # max recursion depth
                print("Fatal error:", err, file=sys.stderr)
            except KeyboardInterrupt:
                print("Execution interrupted by user.", file=sys.stderr)
    except KeyboardInterrupt:
        pass
    print("Bye!")


def showHelp(topic=None):
    if topic:
        # If a topic is specified, show help text for that topic
        found = False
        # Check if it is an operator
        for arity, ops in opsByArity.items():
            if topic in ops:
                print()
                print(ARITIES[arity],
                      "operator:",
                      ops[topic].function)
                print("Precedence:", ops[topic].precedence)
                assoc = ops[topic].associativity
                print("Associativity:",
                      ASSOCIATIVITIES[assoc])
                found = True
        if found:
            print()
        if not found:
            # Check if it is a global variable
            if topic in DEFAULT_VARS:
                print("Global variable:",
                      repr(DEFAULT_VARS[topic]))
                found = True
        # TODO: help text for other things
        if not found:
            print("No help text found for", topic)
    else:
        # Otherwise, show general help text
        print(REPL_HELP_TEXT)


if __name__ == "__main__":
    main()

