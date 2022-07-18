module PSST.Parser (strSolParse, strSolParseR) where
import PSST.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Prim hiding (State, try)
import Control.Monad
import qualified Data.Maybe
import Data.List

type Parser = ParsecT String () Identity

--- ### Helper Info
keywords :: [String]
keywords = ["extract", "replace", "replaceAll", ":e", ":r", ":R", "clear", "check", "state", "solve"]

--- ### Helper Functions for Regex Tree building
regexTreeSeqHelper :: [RegexTree] -> RegexTree
regexTreeSeqHelper [] = EmptySet -- illegal pattern
regexTreeSeqHelper [tree] = tree
regexTreeSeqHelper trees = Sequence trees

regexTreeRepHelper ::  Bool -> Int -> Maybe Int -> [RegexTree] -> [RegexTree]
regexTreeRepHelper lazy start end [] = [EmptySet] -- illegal pattern
regexTreeRepHelper lazy start end [tree] = [Repetition lazy start end tree]
regexTreeRepHelper lazy start end (t:ts) = t : regexTreeRepHelper lazy start end ts

regexTreeBuilder :: [String] -> RegexTree
regexTreeBuilder strings = fst $ regexTreeBuilderAux strings [] 1
    where
        regexTreeBuilderAux :: [String] ->  [RegexTree] -> Int -> (RegexTree, (Int, [String]))
        regexTreeBuilderAux (c:cs) before num = case c of
          "|" -> (BinChoice (regexTreeSeqHelper before) $ fst $ regexTreeBuilderAux cs [] num, (num, []))
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

str :: Parser String
str = do
    symbol "`"
    strings <- many character
    symbol "`"
    return $ concat strings


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
strP = ValExp . RegexVal False <$> regex <?> "a regex string"

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
    symbol "+"
    exp2 <- concatOpP <|> varP <|> strP <?> "a variable or string or another concat operator"
    return (OperatorExp "concat" exp1 (Just exp2) Nothing)

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