# Implementation Guide

## Values

* 4 values:
	1. Regex String: 
    2. Character List String: 
    3. General String: 
    4. Boolean Predicate: 

## Expressions
* 8 expressions:
    1. Literal Value: 
    2. Assignment Expression: 
    3. Variable:
    4. Sequence Expression: 
    5. If Expression: 
    6. Infix Binary Operator:
    7. Builtin Function:
    8. User Function: 

##

## Example:
```
PSST-REPL >>>
PSST-REPL >>> "hello world"
hello world
PSST-REPL >>> x = "hello world"
hello world
PSST-REPL >>> x
hello world
PSST-REPL >>> y = z = x
hello world
PSST-REPL >>> y == z
True
PSST-REPL >>> x = "314"; x == y  
False
PSST-REPL >>> EVEN = '24680'
24680
PSST-REPL >>> ODD = '13579'
13579
PSST-REPL >>> DIGIT = EVEN + ODD
2468013579
PSST-REPL >>> is_subset ODD DIGIT
True
PSST-REPL >>> is_subset ODD EVEN
False
PSST-REPL >>> Number = `^({DIGIT}+?)(?<last>{DIGIT})$` 
`^([2468013579]+?)(<last>[2468013579])$`
PSST-REPL >>> match x Number
True
PSST-REPL >>> match y Number
False
PSST-REPL >>> times_10 number reg = replace number reg `{\1}{\last}0`
???
PSST-REPL >>> times_10 x Number
3140
```