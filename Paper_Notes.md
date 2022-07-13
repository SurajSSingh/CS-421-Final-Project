# Notes on Paper
## Paper Link

## Keyboard Shortcut for special characters (MacOS)
* ∑ = Alt + w
* π = Alt + p
* ∂ = Alt + d (stands in-place of delta)
* £ = Alt + 3 (stands in-place of epsilon)
* † = Alt + t (stands in-place of tau)
* ¶ = Alt + 7 (stands in-place of phi)
* ∫ = Alt + b (stands in-place of gamma)
* Æ = Alt + Shift + ' (stand in-place of fancy-A)
* ‡ = Alt + Shift + 7 (stand in-place of capital tau)

## Section 3: String Constraint Language (SCL) supporting RegExes
### Definitions:
* {Z+} = set of positive integers
* {N} = set of natural numbers
* if n is in {Z+} then [n] := {1 ..= n}
* {∑} = sigma, finite set of letters, aka alphabet
* string = finite sequence of letters of {∑}
* £ = epsilon (empty string)
* {∑*} = set of strings over {∑}  
* {∑£} = union of {∑} and £
* w, w', w" are strings, if w = w'w" then w' is prefix of w for some w"
* w, w', w" are strings, if w = w"w' then w' is suffix of w for some w"

### RegEx (Regular Expression) syntax (Def. 3.1)
e   = {}    # empty set
    | £    # epsilon
    | a     # regular string
    | (e)   # capturing groups
    | [e|e] # choice
    | [e*e] # concatenation
    | [e?]  # greedy optional match
    | [e??] # lazy optional match
    | [e*]  # greedy Kleene star (match as many times as possible)
    | [e*?] # lazy Kleene star (match as few times as possible)
    | [e+]  # greedy match at least one (as many times as possible)
    | [e+?] # lazy match at least one (as few times as possible)
    | [e{m1,m2}] 
    | [e{m1,m2}?]
    where
        a is in {∑}
        n is in {Z+}
        m1, m2 is in {N} with m1 <= m2
        [] for operator precedence

∫ = gamma, {a1 ..= ak} subset of ∑
|e| = length of e
if e and e' are RegEx then 
    e' is a subexpression of e iff
        1. e' = e
        2. e' is a subexpression of e1 or e2 when e = [e1*e2] or [e1|e2]
        3. e' is a subexpression of e1 when e is any of: [e1?], [e1??], [e1*], [e1*?], [e1+], [e1+?], [e1{m1,m2}], [e1{m1,m2}?], or (e1)

### STR (their string constraints language) syntax
definition keywords are modified from the paper, but mean the same thing

¶  = `x = y`
    | `z = x ~ y`
    | `y = extract(i, e, x)`
    | `y = replace(pat, rep, x)`
    | `y = replaceAll(pat,rep,x)`
    | `x is in e`
    | `¶ and ¶`
    | `¶ or ¶`
    | `not ¶`
    where
        * `~` means string concatenation
        * e and pat are in {RegEx}
        * i is in {N}
        * rep is in {REP} with {REP} = union of:
            * {∑} 
            * { $i | i in {N}} where $i is the i-th capturing group (or full matching pattern for 0)
            * {$<, $>} where $< is the prefix before match and $> is the suffix after match

#### Extract
Extracts the matching `i`-th capture group from the match of `e` to `x`. If x is in language of e then return the capture group, or else return an error. If i-th capture group is not matched, even if x is in language of e, then return a `null` type (i.e. not a failed parse, but that matching group is not found): Example: "aa" is in `a+|(a*)` but "aa" match `a+`instead of `(a*)`, so extract(1, `a+|(a*)`, "aa") = Nothing. i = 0 return x on success.
Args:
    * i: Int, the capture group for extraction
    * e: Regex, the regular expression language
    * x: String, the string to extract from
Return: (Either Diagnostic (Maybe String))
    * Right 
        * Just <capture group>
        * Nothing
    * Left <what went wrong>

#### ReplaceAll
Replaces all occurrences of the given pattern `pat` inside string `x` with replacement string `rep`. For `uvw` with `u`, `v`, and `w` sub-expressions of `x` and v matching `pat`; `$0` = `v`, `$<` = `u`, and `$>` = `w`. Values of `u` and `w` are always with respect to the original `x`.
Args: 
* pat: RegEx, The pattern to match against
* rep: REP, The replacement pattern
* x: String, The string to replace
Return: String
    * New, replaced string (may be the same as original)

#### Replace
Same as `ReplaceAll`, except that it only replaces the first (left-most) occurrence

#### Straight Line Property
STR formula ¶ has stright line property if:
1. ¶ contains no negation (i.e. not) nor disjunctions (i.e. or)
2. the equations in ¶ can be ordered into sequence
{STR_sl} = set of straight-line STR formula 

