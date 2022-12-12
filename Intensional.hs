module Intensional where

type IProgram = [IDefinition]

type IDefinition = (String, IExpr)

type IEnv = [Int]

data IExpr
    = IVar String
    | ICall Int String
    | IActuals [IExpr]
    | INum Int
    | IBool Bool
    | IAdd IExpr IExpr
    | IEq IExpr IExpr
    | IIfThenElse IExpr IExpr IExpr
    deriving (Eq)

instance Show IExpr where
    show :: IExpr -> String
    show (IVar x) = x
    show (ICall i x) = "call_" ++ show i ++ "(" ++ x ++ ")"
    show (IActuals xs) = "(" ++ unwords (map show xs) ++ ")"
    show (INum x) = show x
    show (IBool x) = show x
    show (IAdd x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (IEq x y) = "(" ++ show x ++ " = " ++ show y ++ ")"
    show (IIfThenElse x y z) = "(if " ++ show x ++ " then " ++ show y ++ " else " ++ show z ++ ")"

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
        Nothing -> error ("unbound variable '" ++ x ++ "'")
eval' p ts (ICall t n) =
    case lookup n p of
        Just e -> eval' p (t : ts) e
        Nothing -> error ("unbound function " ++ n)
eval' p ts (IActuals es) =
    case ts of
        [] -> error "actuals called with empty environment"
        t : ts' -> eval' p ts' (es !! t)
eval' _ _ (INum n) =
    INum n
eval' _ _ (IBool b) =
    IBool b
eval' p env (IAdd e0 e1) =
    case (eval' p env e0, eval' p env e1) of
        (INum n0, INum n1) -> INum (n0 + n1)
        _ -> error "addition of non-numbers"
eval' p env (IEq e0 e1) =
    case (eval' p env e0, eval' p env e1) of
        (INum n0, INum n1) -> IBool (n0 == n1)
        (IBool b0, IBool b1) -> IBool (b0 == b1)
        _ -> error "'==' can be used only on numbers or booleans"
eval' p env (IIfThenElse c t f) =
    case eval' p env c of
        IBool True -> eval' p env t
        IBool False -> eval' p env f
        _ -> error "condition of 'if' is not a boolean"