# Implementation Guide

## My Regex Syntax
* In-between double quotes ("")
* Represents a set of accepted strings
* Each character is treated literally ("a" --> Literal a)
* If two literals are together, they become a sequence of literals (ab --> Sequence Literal a Literal b)
* If a literal follows a sequence, it is added to the sequence (ab + c --> Sequence Literal a Literal b Literal c)
* Operators:
    * Complement: "~":
        * Inverts the requirements
    * Choice: "|":
    	* Takes SINGLE GROUP before and after (e.g. "ax|by" means Seq ((Lit a) (Choice (x) (b)) (Lit y) )
    * Capture Group: Between parentheses "()"
    * Repetition: anyOf "?" "*" "+", applied to LITERAL before 
        * Greedy Repetition: default, take as many as possible
            * "?": Zero or one
            * "*": At least zero
            * "+": At least one
            * "{m, }": At least m ### NOT INCLUDED
            * "{m,n}": Between m and n (m <= n) ### NOT INCLUDED
        * Lazy Repetition: followed by "?", same meaning as greedy but take as few as possible


* Operations:
    1. Assignment: Modifies the state
        * Var = Var
        * Var = Str
        * Var = Op
    2. Stateless: Returns result without reading or writing to state of the program
        * `concat`
        * `singleton`
        * `subset`
        * `extract`
        * `replace`
        * `replaceAll`
    3. Stateful: Returns result and can read or write to state of the program
        * `state`
        * `clear`
        * `check`/`solve`

## Example Repl
```
StrSol-REPL >>> 

```