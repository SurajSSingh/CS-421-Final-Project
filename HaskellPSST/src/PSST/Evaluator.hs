module PSST.Evaluator (eval) where

import PSST.Core

eval :: Exp -> Env -> Either Diagnostic Exp
eval _ _ = Left $ UnimplementedError "Evaluator"
