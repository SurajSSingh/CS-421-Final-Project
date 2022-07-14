module PSST.Core where
import Data.HashMap.Strict as H

--- ### Environment
type Env = H.HashMap String Exp

data Exp = BoolExp Bool
         | VarExp String

instance Show Exp where
    show (BoolExp b) = show b
    show (VarExp v) = show v
--- ### Values
-- data Val = RegexVal String
--          | CharListVal String
--          | StringVal String
--          | IntVal Int

-- instance Show Val where
--     show (RegexVal regex) = "regex: `" ++ regex ++ "`"
--     show (CharListVal charList) = "alphabet: [" ++ charList ++ "]"
--     show (StringVal string) = string
--     show (IntVal int) = show int

-- --- ### Expressions
-- --- Expressions are defined from page 4
-- data Exp = Epsilon
--          | ValExp Val
--          | VarExp String
--          | AssignmentExp String Exp
--          | BinOpExp String Exp Exp
--          | FuncExp String [String] Exp Env
