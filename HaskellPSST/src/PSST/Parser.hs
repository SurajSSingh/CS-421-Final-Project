module PSST.Parser (strSolParse, strSolParseR) where
import PSST.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Prim hiding (State, try)
import Control.Monad
import qualified Data.Maybe
import Data.List
import Data.Char
import PSST.RTOperations (isRegexSubLang)

type Parser = ParsecT String () Identity

--- ### Helper Info
keywords :: [String]
keywords = ["extract", "replace", "replaceAll", ":e", ":r", ":R", "clear", "check", "state", "solve", "unify", ":n", "subset", ":s", "singleton", ":S",  "union", ":u"]
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



--- ### Helper Functions for Regex Tree building
---     Allows these assumptions:
---         1. Sequence will contain at least two items
---         2. Repetition will apply only to the last item before it
---         3. Epsilon will be removed, unless it's literally the only thing
---         4. Binary choice will have two distinct items, with neither being subset of each other
regexTreeSeqHelper :: [RegexTree] -> RegexTree
regexTreeSeqHelper [] = Epsilon -- Empty sequence is epsilon
regexTreeSeqHelper [tree] = tree -- Return single item as itself (no sequence needed)
regexTreeSeqHelper trees@(t:ts) = case t of
    Epsilon -> regexTreeSeqHelper ts -- Remove epsilon, continue
    _ -> case regexTreeSeqHelper ts of
      EmptySet -> t
      Epsilon -> t
      Sequence rts -> Sequence (t:rts)
      n -> Sequence [t, n]

regexTreeRepHelper ::  Bool -> Int -> Maybe Int -> [RegexTree] -> [RegexTree]
regexTreeRepHelper lazy start end [] = [Epsilon] -- Repeat epsilon as many times as you want, it's still just epsilon
regexTreeRepHelper lazy start end [tree] = [Repetition lazy start end tree] -- Last or only item, put repetition on it
regexTreeRepHelper lazy start end (t:ts) = case t of
    Epsilon -> regexTreeRepHelper lazy start end ts -- Remove epsilon, continue
    _ -> t : regexTreeRepHelper lazy start end ts

regexTreeBinHelper :: RegexTree -> RegexTree -> RegexTree
regexTreeBinHelper Epsilon t2 = Repetition False 0 (Just 1) t2 -- Make t2 optional
regexTreeBinHelper t1 Epsilon = Repetition False 0 (Just 1) t1 -- Make t1 optional
regexTreeBinHelper t1 t2 
    | t2 `isRegexSubLang` t1 = t1                              -- t1 cover t2
    | t1 `isRegexSubLang` t2 = t2                              -- t2 cover t1
    | otherwise = BinChoice t1 t2

regexTreeBuilder :: [String] -> RegexTree
regexTreeBuilder strings = fst $ regexTreeBuilderAux strings [] 1
    where
        regexTreeBuilderAux :: [String] ->  [RegexTree] -> Int -> (RegexTree, (Int, [String]))
        regexTreeBuilderAux (c:cs) before num = case c of
          "|" -> (regexTreeBinHelper simpleBefore after, res)
            where
                simpleBefore = regexTreeSeqHelper before
                (after, res) = regexTreeBuilderAux cs [] num
          "(" -> regexTreeBuilderAux nextCs (before ++ [group]) finalNum
            where
                res = regexTreeBuilderAux cs [] (num+1)
                group = CaptureGroup num $ fst res
                (finalNum, nextCs) = snd res
          ")" -> (regexTreeSeqHelper before, (num, cs))
          "?" -> regexTreeBuilderAux next rTree num
            where
                (rTree, next) = case cs of
                    "?":n -> (regexTreeRepHelper True 0 (Just 1) before, n)
                    _ -> (regexTreeRepHelper False 0 (Just 1) before , cs)
          "+" -> regexTreeBuilderAux next rTree num
            where
                (rTree, next) = case cs of
                    "?":n -> (regexTreeRepHelper True 1 Nothing before, n)
                    _ -> (regexTreeRepHelper False 1 Nothing before , cs)
          "*" -> regexTreeBuilderAux next rTree num
            where
                (rTree, next) = case cs of
                    "?":n -> (regexTreeRepHelper True 0 Nothing before, n)
                    _ -> (regexTreeRepHelper False 0 Nothing before , cs)
          er | er `elem` escapedRegexSymbol   -> regexTreeBuilderAux cs (before ++ [Literal lr]) num
            where
              lr = Data.Maybe.fromMaybe er (stripPrefix "\\" er)
          "." -> regexTreeBuilderAux cs (before ++ [AnyCharLiteral]) num
          "$" -> regexTreeBuilderAux next (before ++ [cgStub]) num
            where
                (cgNum, next) =  numberStringSpliter cs
                cgStub = CaptureGroupStub cgNum
          c   -> regexTreeBuilderAux cs (before ++ [Literal c]) num
        regexTreeBuilderAux [] before num = (regexTreeSeqHelper before, (num, []))

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

-- Adapted from https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
escapeQuote :: Parser String
escapeQuote = do
    d <- char '\\'
    c <- oneOf "\\`"
    return [d, c]

character :: Parser String
character = fmap return (noneOf "\\`") <|> escapeQuote


escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf $
        ['\\','"','0','n','r','v','t','b','f'] -- all regular characters which can be escaped
        ++
        ['(', ')', '|', '?', '*', '+', '$', '.'] -- all regex characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\','"','\0','\n','\r','\v','\t','\b','\f']

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
numP = ValExp . IntVal <$> int <?> "an integer"

--- #### Read a string (regex) value
strP :: ParsecT String () Identity Exp
strP = do
    complement <- optionMaybe $ symbol "!" <|> symbol "~"
    r <- regex <?> "a regex string"
    case complement of
      Nothing -> return $ ValExp $ RegexVal False r
      Just s -> return $ ValExp $ RegexVal True r


--- #### Read a variable name value
varP :: ParsecT String () Identity Exp
varP = VarExp <$> var <?> "a variable"

--- #### Read an assignment from variable name to a simple expression
assignmentP :: ParsecT String () Identity Exp
assignmentP = try $ do
    var <- var <?> "a variable"
    symbol "="
    AssignmentExp var <$> simpleExprP <?> "any simple expression"


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
    return (OperatorExp "subset" exp1 Nothing Nothing)

extractOpP :: ParsecT String () Identity Exp
extractOpP = try $ do
    symbol "extract" <|> symbol ":e"
    maybeSpaceP
    i <- numP <?> "a number"
    maybeSpaceP
    e <- strP <?> "a string"
    maybeSpaceP
    x <- varP <?> "a variable"
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
    variable <- optionMaybe var
    return $ StateOpExp clear variable

checkOpP :: ParsecT String () Identity Exp
checkOpP = try $ do
    clear <- symbol "check" <|> symbol "solve"
    variable <- optionMaybe var
    return $ StateOpExp clear variable

stateOpP :: ParsecT String () Identity Exp
stateOpP = try $ do
    state <- symbol "state"
    return $ StateOpExp state Nothing

simpleExprP :: Parser Exp
simpleExprP = numP
            <|> assignmentP
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
       <|> simpleExprP
       <?> "a value"

exprP :: Parser Exp
exprP = between maybeSpaceP maybeSpaceP rawExprP <* eof

-- Parser
strSolParse :: String -> Either ParseError Exp
strSolParse = parse exprP "Error"

strSolParseR :: String -> Either ParseError RegexTree
strSolParseR = parse regex "Error"