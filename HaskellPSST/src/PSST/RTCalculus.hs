module PSST.RTCalculus where

import PSST.Core
import Data.HashMap.Strict as H

-- andRule :: String -> String -> Env -> Bool
-- andRule [] [] env = False
-- andRule x y env = case H.lookup x env of
--   Nothing -> False
--   Just exps1 -> case H.lookup y env of
--     Nothing -> False
--     Just exps2 -> checkExpr exps1 && checkExpr exps2

-- notOrRule :: String -> String -> Env -> Bool
-- notOrRule [] [] env = False
-- notOrRule x y env = case H.lookup x env of
--     Nothing -> False
--     Just exps1 -> case H.lookup y env of
--       Nothing -> False
--       Just exps2 -> not (checkExpr exps1) || not (checkExpr exps2)

-- notAndRule :: String -> String -> Env -> Bool
-- notAndRule [] [] env = False
-- notAndRule x y env = case H.lookup x env of
--     Nothing -> False
--     Just exps1 -> case H.lookup y env of
--         Nothing -> False
--         Just exps2 -> not (checkExpr exps1) && not (checkExpr exps2)

-- orRule :: String -> String -> Env -> Bool
-- orRule [] [] env = False
-- orRule x y env = case H.lookup x env of
--     Nothing -> False
--     Just exps1 -> case H.lookup y env of
--         Nothing -> False
--         Just exps2 -> checkExpr exps1 || checkExpr exps2

-- doubleNegationRule :: String -> Env -> Bool
-- doubleNegationRule x env = maybe False checkExpr (H.lookup x env)




-- checkRules :: String -> Env -> Bool
-- checkRules x env = True


-- checkExpr :: [Exp] -> Bool
-- checkExpr _ = False