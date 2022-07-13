module PSST.Evaler (psstEval) where
import PSST.Core

psstEval :: Exp -> Either Diagnostic Val
psstEval _ = Left $ UnimplementedError "Evaluation"