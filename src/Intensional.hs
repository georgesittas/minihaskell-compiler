{-# OPTIONS_GHC -Wall #-}

module Intensional where

import Types

eval :: IProgram -> IExpr
eval p = eval' p [] result
  where
    result = case lookup "result" p of
        Just e -> e
        Nothing -> error "no 'result' definition in program"

eval' :: IProgram -> IEnv -> IExpr -> IExpr
eval' p ts (IVar x) =
    case lookup x p of
        Just e -> eval' p ts e
        Nothing -> error ("[Runtime error]: unbound variable '" ++ x ++ "'")
eval' _ _ (INum n) =
    INum n
eval' _ _ (IBool b) =
    IBool b
eval' p ts (IParens e) =
    eval' p ts e
eval' p ts (IIfThenElse c t f) =
    case eval' p ts c of
        IBool True -> eval' p ts t
        IBool False -> eval' p ts f
        _ -> error "[Runtime error]: condition of 'if' is not a boolean"
eval' p ts (ICall t n) =
    case lookup n p of
        Just e -> eval' p (t : ts) e
        Nothing -> error ("[Runtime error]: unbound function '" ++ n ++ "'")
eval' p ts (IActuals es) =
    case ts of
        [] -> error "[Runtime error]: actuals called with empty environment"
        t : ts' -> eval' p ts' (es !! t)
eval' p ts (ICompOp op e0 e1) =
    case (eval' p ts e0, eval' p ts e1) of
        (INum n0, INum n1) ->
            IBool $ case op of
                LtEq -> n0 <= n1
                Lt -> n0 < n1
                GtEq -> n0 >= n1
                Gt -> n0 > n1
                Eq -> n0 == n1
                Neq -> n0 /= n1
        _ -> error "[Runtime error]: comparison of non-numbers"
eval' p ts (IBinaryOp op e0 e1) =
    case (eval' p ts e0, eval' p ts e1) of
        (INum n0, INum n1) ->
            INum $ case op of
                Plus -> n0 + n1
                Mult -> n0 * n1
                Minus -> n0 - n1
                Div -> n0 `div` n1
        _ -> error "[Runtime error]: binary operation on non-numbers"
eval' p ts (IBooleanOp op e0 e1) =
    case (eval' p ts e0, eval' p ts e1) of
        (IBool b0, IBool b1) ->
            IBool $ case op of
                And -> b0 && b1
                Or -> b0 || b1
        _ -> error "[Runtime error]: boolean operation on non-booleans"
eval' p ts (IUnaryOp op e) =
    case eval' p ts e of
        INum n ->
            INum $ case op of
                Positive -> n
                Negative -> -n
                Not -> error "[Runtime error]: unary 'not' on number"
        IBool b ->
            IBool $ case op of
                Positive -> error "[Runtime error]: unary '+' on boolean"
                Negative -> error "[Runtime error]: unary '-' on boolean"
                Not -> not b
        _ -> error "[Runtime error]: unary operation on non-number or non-boolean"
