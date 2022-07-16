module PSST.Parser (strSolParse, strSolParseR) where
import PSST.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

type Parser = ParsecT String () Identity

--- ### Helper Info
keywords :: [String]
keywords = ["extract", "replace", "replaceAll", "in", ":e", ":r", ":R", "clear", "check"]

--- ### Lexers
symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

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

-- parseWith :: Parser a -> String -> Either ParseError a
-- parseWith p = parse p ""

-- -- \d
-- digitP :: Parser Char
-- digitP = oneOf ['0'..'9']

-- -- \d+
-- digitsP :: Parser String
-- digitsP = many1 digitP

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

-- idP :: Parser String
-- idP = liftM2 (:) identFirst (many identRest)
--   where identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z']
--         underScore = oneOf "_"
--         identRest  = identFirst <|> digitP <|> underScore

--- ### Expression parsers

numP :: ParsecT String () Identity Exp
numP = ValExp . IntVal <$> int <?> "an integer"

strP :: ParsecT String () Identity Exp
strP = ValExp . RegexVal False <$> str <?> "a string"

varP :: ParsecT String () Identity Exp
varP = VarExp <$> var <?> "a variable"

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

elementOpP :: ParsecT String () Identity Exp
elementOpP = try $ do
    var <- varP <?> "a variable"
    symbol "in"
    set <- strP <?> "a string"
    return (OperatorExp "element" var (Just set) Nothing)

extractOpP :: ParsecT String () Identity Exp
extractOpP = try $ do
    symbol "extract" <|> symbol ":e"
    i <- numP <?> "a number"
    e <- strP <?> "a string"
    x <- varP <?> "a variable"
    return (OperatorExp "extract" i (Just e) (Just x))

replaceOpP :: ParsecT String () Identity Exp
replaceOpP = try $ do
    symbol "replace" <|> symbol ":r"
    pat <- strP <?> "a string"
    rep <- strP <?> "a string"
    x <- varP <?> "a variable"
    return (OperatorExp "replace" pat (Just rep) (Just x))

replaceAllOpP :: ParsecT String () Identity Exp
replaceAllOpP = try $ do
    symbol "replaceAll" <|> symbol ":R"
    pat <- strP <?> "a string"
    rep <- strP <?> "a string"
    x <- varP <?> "a variable"
    return (OperatorExp "replaceAll" pat (Just rep) (Just x))

clearOpP :: ParsecT String () Identity Exp
clearOpP = try $ do
    clear <- symbol "clear"
    variable <- optionMaybe var
    return $ StateOpExp clear variable

checkOpP :: ParsecT String () Identity Exp
checkOpP = try $ do
    clear <- symbol "check"
    variable <- optionMaybe var
    return $ StateOpExp clear variable

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
rawExprP = elementOpP
       <|> checkOpP
       <|> clearOpP
       <|> simpleExprP
       <?> "a value"

exprP :: Parser Exp
exprP = between maybeSpaceP maybeSpaceP rawExprP <* eof

-- Parser
strSolParse :: String -> Either ParseError Exp
strSolParse = parse exprP "Error"

strSolParseR :: String -> Either ParseError RegexTree
strSolParseR = parse regex "Error"