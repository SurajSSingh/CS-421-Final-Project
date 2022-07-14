module PSST.Parser (strSolParse) where
import PSST.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

type Parser = ParsecT String () Identity

-- Lexemes



-- Parser
strSolParse :: String -> Either Diagnostic Exp
strSolParse x = Left $ UnimplementedError "Parser"