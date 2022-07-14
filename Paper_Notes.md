# Notes on Paper
## Paper Link

## Project Idea
* Simulate PSST with Monad
* Use this to create a solver for straight-line STR formulas 

### Program Commands
* Expressions
    * RegEx Expressions
        * Variable Assignment
        * Concatenate
        * Extract
        * Replace
        * ReplaceAll
        * Is-In (element of)
        * Conjunction (sequence)
    * Commands
        * Quit/Exit: Leaves the program.
        * Check: Check if the current state of the program is satisfiable or unsatisfiable.
        * Clear: Clear the current state of the program
    * Not included because of straight-line constraint
        * Disjunction
        * Negation

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
STR formula ¶ has straight line property if:
1. ¶ contains no negation (i.e. not) nor disjunctions (i.e. or)
2. the equations in ¶ can be ordered into sequence
{STR_sl} = set of straight-line STR formula 

## Section 4: Semantics of String Functions via PSST

### Finite State Automata (Def 4.1)
Finite-state Automata (FA) over ∑ is Æ
Æ = (∑, Q, q0, F, ∂) where:
* Q = finite set of states
* q0 = the initial state, q0 is in Q
* F = the set of final states, F is subset of Q
* ∂ = the transition relations, ∂ is subset of set of all (Q * ∑ * Q) for all Q, ∑

#### Extra FA definition 
`w` is an input string
`Æ` is a FA

1. Run of `Æ` on `w`: a sequence of q_0,a_1,q_1,a_2...a_n,q_n where w = a_1a_2...a_n and (q_j-1, a_j, q_j) is in ∂ for every j in [n]. 
    * A run is an `accepting run` if q_n is in F
2. `w` is `accepted` by `Æ` if there is an accepting run of Æ on w
3. The language `recognized` by `Æ` represented by `Lang(Æ)` is the set of all strings accepted by Æ.
4. The size of `A` represented as`|Æ|` is the cardinality of `∂` in `Æ`
5. `Q-bar` is the set of sequences of non-repetitive elements from `Q` in `Æ`. Sequence of form: (q_1, ..., q_n). Length of each sequence in `Q-bar` is bounded by `|Q|`.
6. If P is in `Q-bar` and q is in `Q`, then q is in P iff there is q = q_i for some i in [n].
7. If P_1 = {q'_1, ..., q'_n} and P_2 = {q'_1, ..., q'_n} and both are in `Q-bar`, then P_1 intersect P_2 == {} (i.e. empty set) if {q_1, ..., q_m} intersect {q'_1, ..., q'_n} == {} 

### Prioritized Streaming String Transducers (PSST) (Def 4.2)
‡ is a PSST
‡ = (Q, ∑, X, ∂, †, E, q0, F) where
* Q is a finite set of states
* ∑ is the input and output alphabet
* X is a finite set of string variables
* ∂ defines the non-epsilon transition with their priorities (from highest to lowest). ∂ is in {Q * ∑ -> Q-bar}
* † is in {Q -> Q-bar * Q-bar} where q is in Q, if †(q) = (P_1;P_2) then P_1 intersect P_2 == {}. †(q) specifies the epsilon-transition at q with P_1 having higher priority than non-epsilon transition out of q and P_2 having lower priority than non-epsilon transition out of q.
* E associates a string variable assignment function with each transition. E is a partial function from {Q * ∑£ * Q} to {X -> (union of X and ∑)*} where the domain is a set of tuples (q, a, q') satisfying either:
    1. a is in ∑ and q' is in ∂(q, a)
    2. a is £ and q' is in †(q)
* q0 is the initial state, q0 is in Q
* F is the output function, which is a partial function from Q to (union of X and ∑)*

Note:
* Given †(q) = (P_1;P_2)
    * π1(†(q)) = P_1
    * π2(†(q)) = P_2
* Size of ‡, denoted as `|‡|`, is the 
    sum with {all (q, a, q') in domain of E} of (
        the sum with {x in X} of (
            |E(q,a,q')(x)|
        )
    ) where |E(q,a,q')(x)| is the number of symbols from union of X and ∑. 
* A PSST ‡ is `copyless` if for each transition (q, a, q') in ‡ and each x in X, x occurs in (E(q,a,q')(x')) {x' in X} at most once. 
* A PSST ‡ is `copyful` if its not copyless
* A run of ‡ on string w is a sequnce of q_0,a_1,s_1,q_1,...,a_m,s_m,q_m such that 
    1. for each i in [m], either
        1. a_i is in ∑, q_i is in ∂(q_i-1, a_i), and s_i = E(q_i-1, a_i, q_i)
        2. a_i = £, q_i is in †(q_i-1), and s_i = E(q_i-1, £, q_i)  
    2. for every subsequence q_i,a_i+1,s_i+1,q_i+1,...,a_j,s_j,q_j such that i < j and a_i+1 = ... = a_j = £, it holds that each epsilon-transition occurs at most once in it, i.e. for every k,l: i ≤ k < l < j, (q_k,q_k+1) ≠ (q_l, q_l+1) 
* ∂(q,a) may = (), meaning there is no a-transition out of q
* For any pair of runs, R and R', R has higher priority that R' if either:
    1. 
    2. 
* An `accepting run` of ‡

#### Example PSST for extract 1st group of (\d+)(\d*)
‡ is where
* ∑ = {0, ..., 9}
* X = {x1, x2}
* Q = {q0, q1, q2, q3, q4}
* F(q_4) = x1
* ∂ = 
{
    (q0, £) -> (q1), 
    (q1, l) -> (q2) for l in ∑,
     
}
* † = 
* E = 