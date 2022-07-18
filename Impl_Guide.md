# Implementation Guide

## My Regex Syntax
* In-between double quotes ("")
* Each character is treated literally ("a" --> Literal a)
* If two literals are together, they become a sequence of literals (ab --> Sequence Literal a Literal b)
* If a literal follows a sequence, it is added to the sequence (ab + c --> Sequence Literal a Literal b Literal c)
* Operators:
    * Choice: "|":
    	* Takes SEQUENCE before and after (not just literals, e.g. "ax|by" means Choice (ax) (by) )
    * Capture Group: Between parentheses "()"
    * Repetition: anyOf "?" "*" "+", applied to LITERAL before 
        * Greedy Repetition: default, take as many as possible
            * "?": Zero or one
            * "*": At least zero
            * "+": At least one
            * "{m, }": At least m ### NOT INCLUDED
            * "{m,n}": Between m and n (m <= n) ### NOT INCLUDED
        * Lazy Repetition: followed by "?", same meaning as greedy but take as few as possible
* Operator Precedence:
    * Capture Group
    * Choice (keep going until you reach the end or hit the begin/end of a capture group)
    * Repetition (look just before for application)

## Example Repl
```
StrSol-REPL >>> 

```