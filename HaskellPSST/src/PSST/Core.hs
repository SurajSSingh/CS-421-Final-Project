module PSST.Core where
import Data.HashMap.Strict as H

--- ### Environment
type Env = H.HashMap String [Exp]

--- ### Values
data Val = BoolVal Bool
         | IntVal Int
         | RegexVal String
         | Null
         deriving (Eq)

instance Show Val where
    show (BoolVal b) = show b
    show (IntVal i) = show i
    show (RegexVal r) = show r
    show Null = show "NULL"

--- ### Expressions
data Exp = ValExp Val
         | VarExp String
         | AssignmentExp String Exp
--- ### Operations:
--- #### Concatenation
--- #### Conjunction
--- #### Element-of
--- #### Concatenation
--- #### Extract
--- #### Replace
--- #### ReplaceAll
         | OperatorExp String Val Exp Exp
         deriving (Eq)


instance Show Exp where
    show (ValExp v) = show v
    show (VarExp v) = show v
    show (AssignmentExp var exp) = "Set " ++ show var ++ " to " ++ show exp
    show (OperatorExp op v e1 e2) = "Operation " ++ show e1 ++ " and " ++ show e2

data Diagnostic = UnimplementedError String
                | ParseError String
                deriving (Eq)

instance Show Diagnostic where
    show (UnimplementedError x) = x ++ " Unimplemented"
    show (ParseError x) = "Parsing Error: " ++ x
