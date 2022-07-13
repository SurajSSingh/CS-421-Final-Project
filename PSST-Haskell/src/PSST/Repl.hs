-- Repl code adapted from https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2
module PSST.Repl
    (repl
    ) where

import PSST.Core(Exp (..), Val (..), Diagnostic (..))
import System.IO
import Control.Monad

readString :: IO String
readString = putStr "PSST-REPL >>> "
       >> hFlush stdout
       >> getLine

parse :: String -> Exp
parse str = ValExp (BoolVal False)

eval :: Exp -> Either Diagnostic Val
eval (ValExp val) = Right val
eval _ = Left UnimplementedError

repl :: IO () -> IO ()
repl main = do
    input <- readString

    unless (input == "exit" || input == "stop" || input == "end" || input == "quit")
         $ case eval (parse input) of
            Right val -> print val
            Left err -> print err
         >> main