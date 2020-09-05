
GCD of two numbers:

    Wb%:aSaba

Explanation:

    Wb%:a      Assign b%a to b and loop while nonzero:
         Sab    Swap a and b
            a  After the loop, print a

Three quine strategies:

    " X2RsC34.s" X2RsC34.s  Duplicate the string and replace each space with double quote followed by space
    Y"Y yRsRPy"yRsRPy       Repr substitution, as in the standard Python quine
    V Y"`V Y`.RPy"          Using eval, with a Pattern to allow a string-like object without double quotes

First `a` Fibonacci numbers, starting with 1:

    La{Po+:xSox}

Arithmetic mean of input:

    $+g/#g

Explanation:

      g      List of cmdline args
       /#g   Divide each element of the list by the length of the list
    $+       Sum the result (fold on addition)

Three different ways to do factorial:

    a?a*(fa-1)1   Recursive...
    Fi\,ao*:io    Iterative...
    $*\,a         But fold is the best!

FizzBuzz:

    LhP J["Fizz""Buzz"]X!*++i%^35|i

Explanation:

    Lh                               Loop 100 times:
      P                              Print this expression:
                              ^35    35, split into a list of characters [3;5]
                          ++i%       Increment i and take its new value mod each of those numbers
                        !*           Logically negate each value (0 -> 1, nonzero -> 0)
         ["Fizz""Buzz"]X             String-multiply "Fizz" and "Buzz", itemwise, by the above
                                     The result is a list containing "Fizz" or "" depending on i%3
                                     and "Buzz" or "" depending on i%5
        J                            Join that list into a string
                                 |i  Logical or with i (i.e. use the number if the resulting string is "")

[Translate alphanumeric phone numbers](http://codegolf.stackexchange.com/q/21327/16766)

    {aQ'z?9aNz?5*Aa//16-28a}Ma

[Is a number divisible by all of its digits?](http://codegolf.stackexchange.com/q/41902/16766)

    0=$+a%^a

Explanation:

          ^a  Split num (as string) into an array of its digits                              [1;2;8]  [2;0]   [3;2]
        a%    Take num mod each of those digits; if a digit is zero, the result will be nil  [0;0;0]  [0;()]  [2;0]
      $+      Sum the resulting list (note: summing a list containing nil results in nil!)   0        ()      2
    0=        Iff the sum equals 0, return 1 (true); otherwise (>0 or nil), return 0 (false) 1        0       0

[Build nested lists](http://codegolf.stackexchange.com/q/47351/16766)

    a?--a?[0(fa)+1][0]l   20 bytes counting -p flag

Explanation:

    a?                l   If input is 0, return empty list
      --a?         [0]    Else decrement input; if it is now 0, return [0]
            (fa)+1        If --a was not 0, recurse and add 1 memberwise to the result...
          [0      ]       ... and make it the second item in a new outer list
                          The recursive main function thus builds up lists like [0] -> [0;[1]] -> [0;[1;[2]]] etc.
