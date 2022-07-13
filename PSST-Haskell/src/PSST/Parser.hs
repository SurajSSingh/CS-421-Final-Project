-- Adapted from MP4 Parser.hs and MP5 Parser.hs
module PSST.Parser (psstParser) where
import PSST.Core

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Control.Monad

type Parser = ParsecT String () Identity

-- ### Regex Definition (from 3.1)


--- ### Lexicals
digitP :: Parser Char
digitP = oneOf ['0'..'9']

digitsP :: Parser String
digitsP = many1 digitP

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

idP :: Parser String
idP = liftM2 (:) identFirst (many1 identRest)
  where identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z']
        identRest  = identFirst <|> digitP <|> oneOf "_"

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

singleQuote :: Parser a -> Parser a
singleQuote p =
    do symbol "\'"
       pp <- p
       symbol "\'"
       return pp

doubleQuote :: Parser a -> Parser a
doubleQuote p =
    do symbol "\""
       pp <- p
       symbol "\""
       return pp

backQuote :: Parser a -> Parser a
backQuote p =
    do symbol "`"
       pp <- p
       symbol "`"
       return pp

--- ### Expressions
-- valExp:: Parser Val
-- valExp = 
--     do
--         regex <- backQuote
--         return   

varExp :: Parser Exp
varExp = do VarExp <$> idP

psstParser :: String -> Exp
psstParser str = ValExp (PredicateVal False)
    -- where
    --     x = varExp str