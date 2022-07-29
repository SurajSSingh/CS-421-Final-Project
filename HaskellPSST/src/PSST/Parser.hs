--- # Parser Module
--- ## Adapted from 
module PSST.Parser (strSolParse, strSolParseRegex) where
import PSST.Core
    ( Exp(..),
      RegexNode(..),
      epsilonNode,
      anyCharNode,
      sCharNode,
      kleeneStarNode,
      kleenePlusNode,
      optionalNode,
      captureGroupStub,
      wrapNodeInCaptureGroup )

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Prim hiding (State, try)
import Control.Monad
import qualified Data.Maybe
import Data.List
import Data.Char
import Data.Maybe (fromMaybe, isJust)

--- ## 
type Parser = ParsecT String () Identity

--- ### Helper Info
--- #### Keywords: word that perform operations, cannot be variable names
keywords :: [String]
keywords = ["extract", "replace", "replaceAll", ":e", ":r", ":R", "clear", "check", "state", "solve", "unify", ":n", "subset", ":s", "singleton", ":S",  "union", ":u"]
--- #### Special symbols used by regex, required to be escaped if they are used
regexSpecialSymbols :: [Char]
regexSpecialSymbols = ['(', ')', '|', '$', '.', '~', '?', '*', '+', '{', '}']
--- #### Digits of integer, probably a better way of doing this
digits :: [String]
digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

--- ### Lexers
--- #### Notes to self:
---     * spaces = 0 or more spaces

--- #### Get a symbol
symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

--- #### Get an integer
int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

--- #### Get a variable identifier
var :: Parser String
var = try $ do
    v1 <- letter <?> "an identifier"
    vs <- many (letter <|> digit <|> oneOf "_-") <?> "an identifier"
    spaces
    let v = v1:vs
    if v `elem` keywords
    then fail ("got a keyword: " ++ show v ++ " in " ++ show keywords)
    else return v

--- Adapted from https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
--- #### Get an escaped character
escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf $
        ['\\','"','0','n','r','v','t','b','f'] -- all regular characters which can be escaped
        ++
        regexSpecialSymbols -- all regex characters which can be escaped
    return [d, c]

--- #### Get a non-escaped character
nonEscape :: Parser Char
nonEscape = noneOf $ ['\\','"','\0','\n','\r','\v','\t','\b','\f'] ++ regexSpecialSymbols

--- #### Get a character that is either escaped special character or regular non-escaped character
rCharacter :: Parser String
rCharacter = fmap return nonEscape <|> escape

--- #### Get a possible, many whitespace characters
maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

--- ### Regex Parse
--- #### Create Literal Character Node by reading a single character
literalCharNodeParse :: Parser RegexNode
literalCharNodeParse = do
    litChar <- rCharacter
    case litChar of
        ""  -> return epsilonNode
        c   -> return $ sCharNode litChar

--- #### Create an Any Character Literal Node by reading a dot
anyCharNodeParse :: Parser RegexNode
anyCharNodeParse = do
    char '.'
    return anyCharNode

--- #### Create a Complement Node by reading a negation symbol and getting the next node
complementNodeParse :: Parser RegexNode
complementNodeParse = do
    char '~'  <?> "a complement symbol"
    n <- nodeParse <?> "a regex node"
    return $ ComplementNode n

--- #### Create a Choice Node by reading the previous node, then a pipe symbol, then the next node 
choiceNodeParse :: Parser RegexNode
choiceNodeParse = try $ do
    a <- forwardOnlyNodeParse <?> "a regex node"
    char '|' <?> "a choice symbol"
    b <- nodeParse <?> "a regex node"
    return $ ChoiceNode a b

--- #### Create a Repetition Node by reading the previous node and then a symbolic repetition symbol
symbolRepetitionNodeParse :: Parser RegexNode
symbolRepetitionNodeParse = try $ do
    n <- forwardOnlyNodeParse <?> "a regex node"
    r <- oneOf ['*', '+', '?'] <?> "a repeat symbol"
    l <- optionMaybe (char '?') <?> "an optional lazy symbol"
    case r of
        '*' -> return $ kleeneStarNode (isJust l) n
        '+' -> return $ kleenePlusNode (isJust l) n
        '?' -> return $ optionalNode (isJust l) n
        _ -> unexpected "unknown repeating character"

--- #### Create a Repetition Node by reading the previous node and then reading a curly brace with the repeating start and maybe end
curlyRepetitionNodeParse :: Parser RegexNode
curlyRepetitionNodeParse = try $ do
    n <- forwardOnlyNodeParse <?> "a regex node"
    char '{' <?> "open curly repetition"
    x <- int <?> "a starting number"
    c <- optionMaybe $ char ','
    maybeSpaceP
    y <- optionMaybe int <?> "an optional ending number"
    char '}' <?> "close curly repetition"
    l <- optionMaybe (char '?') <?> "an optional lazy symbol"
    case c of
      -- Exact repetition
      Nothing -> return $ RepetitionNode (isJust l) x (Just x) n
      -- Bounded repetition
      Just _ -> return $ RepetitionNode (isJust l) x y n

--- #### Create a Capture Group Sequence by reading an opening parentheses, then reading multiple nodes, and finally a closing parentheses
captureGroupParse :: Parser RegexNode
captureGroupParse = do
    char '('
    n <- many1 nodeParse
    char ')'
    return $ CaptureGroupSequence 0 n

--- #### Create a Capture Group Stub (filled in for replacement) by reading the stub symbol and then a number
captureGroupStubParse :: Parser RegexNode
captureGroupStubParse = do
    char '$'
    captureGroupStub <$> int <?> "a number"

--- #### Parse nodes that only need to be parsed forwards without relying on previous nodes
forwardOnlyNodeParse :: Parser RegexNode
forwardOnlyNodeParse = complementNodeParse
                <|> captureGroupParse
                <|> captureGroupStubParse
                <|> anyCharNodeParse
                <|> literalCharNodeParse
                <?> "a valid non-choice regex node"

--- #### Parse any kind of nodes, even if they require previous nodes to work 
nodeParse :: Parser RegexNode
nodeParse = choiceNodeParse
          <|> symbolRepetitionNodeParse
          <|> curlyRepetitionNodeParse
          <|> forwardOnlyNodeParse
          <?> "a valid regex node"

--- #### Parse a regex string by reading in-between the quote characters and extracting the final regex node.
---      A special case for empty string is that it gets turned into an epsilon node 
regexStringParse :: Parser RegexNode
regexStringParse = try $ do
    char '"' <?> "opening quote"
    nodes <- many nodeParse
    char '"' <?> "closing quote"
    case nodes of
      [] -> return $ wrapNodeInCaptureGroup [epsilonNode]
      _ -> return $ wrapNodeInCaptureGroup nodes

--- ### Expression parsers

--- #### Read a number (integer) value
numP :: ParsecT String () Identity Exp
numP = IntExp <$> int <?> "an integer"

--- #### Read a string (regex) value
strP :: ParsecT String () Identity Exp
strP = do
    r <- regexStringParse <?> "a regex string"
    return $ RegexExp r

--- #### Read a variable name value
varP :: ParsecT String () Identity Exp
varP = VarExp <$> var <?> "a variable"

--- #### Read an assignment from variable name to a simple expression
assignmentP :: ParsecT String () Identity Exp
assignmentP = try $ do
    var <- var <?> "a variable"
    symbol "="
    val <- simpleExprP <?> "any simple expression"
    return $ AssignmentExp var val

--- #### Read the concatenation operation
concatOpP :: ParsecT String () Identity Exp
concatOpP = try $ do
    exp1 <- varP <|> strP <?> "a variable or string"
    maybeSpaceP
    plus <- symbol "+"
    maybeSpaceP
    exp2 <- concatOpP <|> varP <|> strP <?> "a variable or string or another concat operator"
    maybeSpaceP
    return (OperatorExp "concat" exp1 (Just exp2) Nothing)

--- #### Read the union operation
unionOpP :: ParsecT String () Identity Exp
unionOpP = try $ do
    union <- symbol "union" <|> symbol ":u"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    exp2 <- strP <|> unionOpP <?> "a string or another union operator"
    maybeSpaceP
    return (OperatorExp "union" exp1 (Just exp2) Nothing)

--- #### Read the unify (intersection) operation
unifyOpP :: ParsecT String () Identity Exp
unifyOpP = try $ do
    unify <- symbol "unify" <|> symbol ":n"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    exp2 <- unifyOpP <|> strP <?> "a string or another unify operator"
    maybeSpaceP
    return (OperatorExp "unify" exp1 (Just exp2) Nothing)

--- #### Read the singleton operation
singletonOpP :: ParsecT String () Identity Exp
singletonOpP = try $ do
    unify <- symbol "single" <|> symbol ":S"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "singleton" exp1 Nothing Nothing)

--- #### Read the subset operation
subsetOpP :: ParsecT String () Identity Exp
subsetOpP = try $ do
    unify <- symbol "subset" <|> symbol ":s"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    exp2 <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "subset" exp1 (Just exp2) Nothing)

--- #### Read the extraction operation
extractOpP :: ParsecT String () Identity Exp
extractOpP = try $ do
    symbol "extract" <|> symbol ":e"
    maybeSpaceP
    i <- numP <?> "a number"
    maybeSpaceP
    e <- strP <?> "a string"
    maybeSpaceP
    x <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "extract" i (Just e) (Just x))

--- #### Read the replacement operation
replaceOpP :: ParsecT String () Identity Exp
replaceOpP = try $ do
    symbol "replace" <|> symbol ":r"
    maybeSpaceP
    pat <- strP <?> "a string"
    maybeSpaceP
    rep <- strP <?> "a string"
    maybeSpaceP
    x <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "replace" pat (Just rep) (Just x))

--- #### Read the replace all operation
replaceAllOpP :: ParsecT String () Identity Exp
replaceAllOpP = try $ do
    symbol "replaceAll" <|> symbol ":R"
    maybeSpaceP
    pat <- strP <?> "a string"
    maybeSpaceP
    rep <- strP <?> "a string"
    maybeSpaceP
    x <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "replaceAll" pat (Just rep) (Just x))

--- #### Read the clear operation
clearOpP :: ParsecT String () Identity Exp
clearOpP = try $ do
    clear <- symbol "clear"
    variable <- optionMaybe varP
    return $ StateOpExp clear variable

--- #### Read the check/solve operation
checkOpP :: ParsecT String () Identity Exp
checkOpP = try $ do
    clear <- symbol "check" <|> symbol "solve"
    variable <- optionMaybe varP
    return $ StateOpExp clear variable

--- #### Read the state operation
stateOpP :: ParsecT String () Identity Exp
stateOpP = try $ do
    state <- symbol "state"
    variable <- optionMaybe varP
    return $ StateOpExp state variable

--- #### Read simple (composable) expression
simpleExprP :: Parser Exp
simpleExprP = numP
            <|> unionOpP
            <|> unifyOpP
            <|> concatOpP
            <|> extractOpP
            <|> replaceOpP
            <|> replaceAllOpP
            <|> strP
            <|> varP
            <?> "a simple value"

--- #### Read raw expression (may or may not be composable)
rawExprP :: Parser Exp
rawExprP = checkOpP
       <|> clearOpP
       <|> stateOpP
       <|> subsetOpP
       <|> singletonOpP
       <|> assignmentP
       <|> simpleExprP
       <?> "a value"

--- #### Read an expression
exprP :: Parser Exp
exprP = between maybeSpaceP maybeSpaceP rawExprP <* eof

--- ### Parser
--- #### Try to parse an expression
strSolParse :: String -> Either ParseError Exp
strSolParse = parse exprP "Error"

--- #### Try to parse a regex
strSolParseRegex :: String -> Either ParseError RegexNode
strSolParseRegex = parse regexStringParse "Error"