module PSST.Parser (strSolParse, strSolParseRegex) where
import PSST.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Prim hiding (State, try)
import Control.Monad
import qualified Data.Maybe
import Data.List
import Data.Char
import Data.Maybe (fromMaybe, isJust)

type Parser = ParsecT String () Identity

--- ### Helper Info
keywords :: [String]
keywords = ["extract", "replace", "replaceAll", ":e", ":r", ":R", "clear", "check", "state", "solve", "unify", ":n", "subset", ":s", "singleton", ":S",  "union", ":u"]
regexSpecialSymbols :: [Char]
regexSpecialSymbols = ['(', ')', '|', '$', '.', '~', '?', '*', '+', '{', '}']
digits :: [String]
digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

numberStringSpliter :: [String] -> (Maybe Int, [String])
numberStringSpliter = aux []
    where
        aux :: [String] -> [String] -> (Maybe Int, [String])
        aux [] [] = (Nothing, [])
        aux [] c = (Nothing, c)
        aux n [] = (Just (read (intercalate "" n) :: Int), [])
        aux n c@(x:xs) = if x `elem` digits then aux (n ++ [x]) xs else (Just (read (intercalate "" n) :: Int), c)


regexTreeBinHelper :: RegexTree -> RegexTree -> RegexTree
regexTreeBinHelper t1 t2
    | t2 `isSubtree` t1 = t1                              -- t1 cover t2
    | t1 `isSubtree` t2 = t2                              -- t2 cover t1
    | otherwise = Choice t1 t2

regexTreeBuilder :: [String] -> RegexTree
regexTreeBuilder strings = CaptureGroup 0 (fst $ regexTreeBuilderAux strings [] 1)
    where
        regexTreeBuilderAux :: [String] ->  [RegexTree] -> Int -> (RegexTree, (Int, [String]))
        regexTreeBuilderAux [] before num = (listToRegexTree before, (num, []))
        regexTreeBuilderAux (c:cs) before num = case c of
            --- NOTE: Sequence are constructed at the end and Empty Set can never be constructed from strings
            --- Complement
            "~" -> regexTreeBuilderAux nextCs (before ++ [Complement rTree]) nextNum
                where
                    (rTree, (nextNum, nextCs)) = regexTreeBuilderAux cs [] num
            --- Choice
            "|" -> (regexTreeBinHelper updatedBefore updatedAfter, result)
                where
                    updatedBefore = listToRegexTree before
                    (updatedAfter, result) = regexTreeBuilderAux cs [] num
            --- CaptureGroup
            "(" -> regexTreeBuilderAux nextCs (before ++ [CaptureGroup num rTree]) finalNum
                where
                    (rTree, (finalNum, nextCs)) = regexTreeBuilderAux cs [] (num+1)
            ")" -> (listToRegexTree before, (num, cs))
            "$" -> regexTreeBuilderAux nextCs (before ++ [CaptureGroup cgStubNum emptySet]) num
                where
                    (cgNum, nextCs) = numberStringSpliter cs
                    cgStubNum = -(fromMaybe 0 cgNum)
            --- Default: Literal
            "" -> regexTreeBuilderAux cs (before ++ [Literal epsilon]) num
            "." -> regexTreeBuilderAux cs (before ++ [Literal anyCharacter]) num
            c -> regexTreeBuilderAux cs (before ++ [Literal $ Right c]) num

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


--- Regex Parse
literalCharNodeParse :: Parser RegexNode
literalCharNodeParse = do
    litChar <- rCharacter
    case litChar of
        ""  -> return epsilonNode
        c   -> return $ sCharNode litChar

anyCharNodeParse :: Parser RegexNode
anyCharNodeParse = do
    char '.'
    return anyCharNode

complementNodeParse :: Parser RegexNode
complementNodeParse = do
    char '~'  <?> "a complement symbol"
    n <- nodeParse <?> "a regex node"
    return $ ComplementNode n

choiceNodeParse :: Parser RegexNode
choiceNodeParse = try $ do
    a <- forwardOnlyNodeParse <?> "a regex node"
    char '|' <?> "a choice symbol"
    b <- forwardOnlyNodeParse <?> "a regex node"
    return $ ChoiceNode a b

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

captureGroupParse :: Parser RegexNode
captureGroupParse = do
    char '('
    n <- many1 nodeParse
    char ')'
    return $ CaptureGroupSequence 0 n

captureGroupStubParse :: Parser RegexNode
captureGroupStubParse = do
    char '$'
    captureGroupStub <$> int <?> "a number"

forwardOnlyNodeParse :: Parser RegexNode
forwardOnlyNodeParse = complementNodeParse
                <|> captureGroupParse
                <|> captureGroupStubParse
                <|> anyCharNodeParse
                <|> literalCharNodeParse
                <?> "a valid non-choice regex node"

nodeParse :: Parser RegexNode
nodeParse = choiceNodeParse
          <|> symbolRepetitionNodeParse
          <|> curlyRepetitionNodeParse
          <|> forwardOnlyNodeParse
          <?> "a valid regex node"

regexStringParse :: Parser RegexNode
regexStringParse = try $ do
    char '"' <?> "opening quote"
    nodes <- many nodeParse
    char '"' <?> "closing quote"
    case nodes of
      [] -> return $ CaptureGroupSequence 0 [epsilonNode]
      _ -> return $ CaptureGroupSequence 0 $ snd $ renumberCaptureGroup 1 nodes


-- Adapted from https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf $
        ['\\','"','0','n','r','v','t','b','f'] -- all regular characters which can be escaped
        ++
        regexSpecialSymbols -- all regex characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf $ ['\\','"','\0','\n','\r','\v','\t','\b','\f'] ++ regexSpecialSymbols

rCharacter :: Parser String
rCharacter = fmap return nonEscape <|> escape

regex :: Parser RegexTree
regex = try $ do
    char '"' <?> "OPEN QUOTE"
    strings <- many rCharacter
    char '"' <?> "CLOSE QUOTE"
    return $ regexTreeBuilder strings

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"


--- ### Expression parsers

--- #### Read a number (integer) value
numP :: ParsecT String () Identity Exp
numP = IntExp <$> int <?> "an integer"

--- #### Read a string (regex) value
strP :: ParsecT String () Identity Exp
strP = do
    -- complement <- optionMaybe $ symbol "!" <|> symbol "~"
    r <- regex <?> "a regex string"
    return $ RegexExp r
    -- case complement of
    --   Nothing -> return $ ValExp $ RegexVal False r
    --   Just s -> return $ ValExp $ RegexVal True r


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


concatOpP :: ParsecT String () Identity Exp
concatOpP = try $ do
    exp1 <- varP <|> strP <?> "a variable or string"
    maybeSpaceP
    plus <- symbol "+"
    maybeSpaceP
    exp2 <- concatOpP <|> varP <|> strP <?> "a variable or string or another concat operator"
    maybeSpaceP
    return (OperatorExp "concat" exp1 (Just exp2) Nothing)

unionOpP :: ParsecT String () Identity Exp
unionOpP = try $ do
    union <- symbol "union" <|> symbol ":u"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    exp2 <- strP <|> unionOpP <?> "a string or another union operator"
    maybeSpaceP
    return (OperatorExp "union" exp1 (Just exp2) Nothing)

unifyOpP :: ParsecT String () Identity Exp
unifyOpP = try $ do
    unify <- symbol "unify" <|> symbol ":n"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    exp2 <- unifyOpP <|> strP <?> "a string or another unify operator"
    maybeSpaceP
    return (OperatorExp "unify" exp1 (Just exp2) Nothing)

singletonOpP :: ParsecT String () Identity Exp
singletonOpP = try $ do
    unify <- symbol "single" <|> symbol ":S"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "singleton" exp1 Nothing Nothing)

subsetOpP :: ParsecT String () Identity Exp
subsetOpP = try $ do
    unify <- symbol "subset" <|> symbol ":s"
    maybeSpaceP
    exp1 <- strP <?> "a string"
    maybeSpaceP
    exp2 <- strP <?> "a string"
    maybeSpaceP
    return (OperatorExp "subset" exp1 (Just exp2) Nothing)

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

replaceOpP :: ParsecT String () Identity Exp
replaceOpP = try $ do
    symbol "replace" <|> symbol ":r"
    maybeSpaceP
    pat <- strP <?> "a string"
    maybeSpaceP
    rep <- strP <?> "a string"
    maybeSpaceP
    x <- varP <?> "a variable"
    maybeSpaceP
    return (OperatorExp "replace" pat (Just rep) (Just x))

replaceAllOpP :: ParsecT String () Identity Exp
replaceAllOpP = try $ do
    symbol "replaceAll" <|> symbol ":R"
    maybeSpaceP
    pat <- strP <?> "a string"
    maybeSpaceP
    rep <- strP <?> "a string"
    maybeSpaceP
    x <- varP <?> "a variable"
    maybeSpaceP
    return (OperatorExp "replaceAll" pat (Just rep) (Just x))

clearOpP :: ParsecT String () Identity Exp
clearOpP = try $ do
    clear <- symbol "clear"
    variable <- optionMaybe varP
    return $ StateOpExp clear variable

checkOpP :: ParsecT String () Identity Exp
checkOpP = try $ do
    clear <- symbol "check" <|> symbol "solve"
    variable <- optionMaybe varP
    return $ StateOpExp clear variable

stateOpP :: ParsecT String () Identity Exp
stateOpP = try $ do
    state <- symbol "state"
    return $ StateOpExp state Nothing

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

rawExprP :: Parser Exp
rawExprP = checkOpP
       <|> clearOpP
       <|> stateOpP
       <|> subsetOpP
       <|> singletonOpP
       <|> assignmentP
       <|> simpleExprP
       <?> "a value"

exprP :: Parser Exp
exprP = between maybeSpaceP maybeSpaceP rawExprP <* eof

-- Parser
strSolParse :: String -> Either ParseError Exp
strSolParse = parse exprP "Error"

strSolParseRegex :: String -> Either ParseError RegexNode
strSolParseRegex = parse regexStringParse "Error"