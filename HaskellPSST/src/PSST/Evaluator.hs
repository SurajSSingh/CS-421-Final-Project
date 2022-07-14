module PSST.Evaluator (eval) where

import PSST.Core

eval :: Exp -> Env -> Either Exp Exp
eval _ _ = Right $ BoolExp False
