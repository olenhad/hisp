module Eval (eval,
             Expr
            ) where

import Data.Maybe
import qualified Data.Map as M

type FunctionArgs = [String]
type Environment = M.Map String Result

data Result = IntRes Integer
            | BoolRes Bool
            | FunctionR Environment FunctionArgs Expr
            | NilRes
            | Error
            deriving (Show)

data Expr = Constant Integer
          | Boolean Bool
          | Symbol String
          | ListE Expr Expr
          | Negate Expr
          | Add Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | SymbolDefE String Expr
          | DoE [Expr]
          | IfE Expr Expr Expr
          | Lambda FunctionArgs Expr
          | FuncInvoke String [Expr]
          deriving (Show)




type EvalResult = (Environment, Result)

eval :: Environment -> Expr -> EvalResult

eval e (Constant c) = (e, IntRes c)

eval e (Boolean b) = (e, BoolRes b)

eval env (Symbol name) =
  case M.lookup name env of
    Just res -> (env, res)
    _ -> (env, Error)

eval env (SymbolDefE name expr) =
  case (eval env expr) of
    (env', val) -> (M.insert name val env', NilRes)

eval env (DoE es) =
  foldl (\ (env', _) e -> eval env' e) (env,NilRes) es

eval env (IfE c e1 e2) =
  case (eval env c) of
    (env', BoolRes True) -> eval env' e1
    (env', BoolRes False) -> eval env' e2
    _ -> (env, Error)

eval env (Lambda args body) = (env, FunctionR env args body)

eval env (FuncInvoke name argEs) =
  case M.lookup name env of
    Just (FunctionR fenv argNames body) ->
      (env, snd (eval newEnv body))
      where
        newEnv = M.union (M.fromList
                          (zip argNames
                           (map (\e -> snd $ eval env e) argEs))) fenv
    _ -> (env, Error)


eval en (Negate e) =
  case (eval en e) of
    (en' ,IntRes i) -> (en', IntRes (negate i))
    _ -> (en, Error)

eval env (Add e1 e2) =
  case ((eval env e1), (eval env e2)) of
    ((env1, IntRes i1), (env2, IntRes i2)) -> (M.union env1 env2, IntRes (i1 + i2))
    _ -> (env, Error)

eval env (Multiply e1 e2) =
  case ((eval env e1), (eval env e2)) of
    ((env1, IntRes i1), (env2, IntRes i2)) -> (M.union env1 env2, IntRes (i1 * i2))
    _ -> (env, Error)

eval env (Divide e1 e2) =
  case ((eval env e1), (eval env e2)) of
    ((env1, IntRes i1), (env2, IntRes i2)) -> (M.union env1 env2, IntRes (quot i1 i2))
    _ -> (env, Error)

eval env (And e1 e2) =
  case (eval env e1, eval env e2) of
    ((env1, BoolRes t1), (env2, BoolRes t2)) -> (M.union env1 env2, BoolRes (and [t1, t2]))
    _ -> (env, Error)

eval env (Or e1 e2) =
  case (eval env e1, eval env e2) of
    ((env1, BoolRes t1), (env2, BoolRes t2)) -> (M.union env1 env2, BoolRes (or [t1, t2]))
    _ -> (env, Error)




testFunc = eval M.empty
        (DoE [(SymbolDefE "a" (Lambda ["a1","a2"] (Add (Symbol "a1") (Symbol "a2")))),
              (FuncInvoke "a" [(Constant 5), (Constant 7)])])

testClosure = eval M.empty
              (DoE
               [(SymbolDefE "b" (Constant 24)),
                 (SymbolDefE "a" (Lambda ["a1","a2"] (Add
                                                      (Symbol "b")
                                                      (Add (Symbol "a1") (Symbol "a2"))))),
                 (SymbolDefE "b" (Constant 100)),
                 (FuncInvoke "a" [(Constant 5), (Constant 7)])
               ])
