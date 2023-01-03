module Transform (
    transform
) where


import Debug.Trace

import Types

type MapParams = [(String, [String])]  -- Maps "function name" to "parameter array"
type MapIndex = [(String, Int)]        -- Maps "function name" to "current CALL index to be used"

type TransformationUtilities = (IProgram, MapIndex, MapParams)

transform :: FProgram -> IProgram
transform fprog = iprog 
    where 
        util = trace ("phase 1 : printing util " ++ show x) (x)
        x = add_actuals fprog ([], [], [])
        (iprogTmp, _, _) = convert_to_unary fprog util
        iprog = find_and_reverse_actuals iprogTmp  -- iprogTmp

-- 1st pass

-- Parse the FDefinitions and create empty IActuals for each unique parameter of the FProgram.
-- Side effect: Create "mapIndex" and "mapParams"
add_actuals :: FProgram -> TransformationUtilities -> TransformationUtilities
add_actuals (_, []) util = util
add_actuals (res, (fdef : fdefs)) (iProgram, mapIndex, mapParams) =
        add_actuals (res, fdefs) (iProgram', mapIndex', mapParams')
    where
        (fname, fparams, fexpr) = fdef
        iProgram' = parse_def_for_actuals fparams iProgram
        mapIndex' = ((fname, 0) : mapIndex)
        mapParams' = ((fname, fparams) : mapParams)

-- Parse the "Parameter" field of an FDefinition and create empty IActuals for each param.
parse_def_for_actuals ::  [String] -> IProgram -> IProgram
parse_def_for_actuals [] iProg = iProg 
parse_def_for_actuals (param : rest) iProg =  ( (param, IActuals []) : iProg )


-- // 1st pass

-- 2nd pass

-- (wrapper) Combine "result" with the rest of "FDefinitions" and transform it to an IProgram
convert_to_unary :: FProgram -> TransformationUtilities -> TransformationUtilities
convert_to_unary (res, fdefs) util =
        convert_fdefs_to_unary totalFdefs util
    where  -- this is a bit asymmetric and inconvenient but w/e
        totalFdefs = ( ("result", [], res) : fdefs )

-- Convert FDefinitions to IDefinitions
convert_fdefs_to_unary :: [FDefinition] -> TransformationUtilities -> TransformationUtilities
convert_fdefs_to_unary [] util = util
convert_fdefs_to_unary ((fname, _, fexpr) : rs)  (iProg, mapIndex, mapParams) = 
        convert_fdefs_to_unary rs (iProg', mapIndex', mapParams)
    where
        (iExpr, tempIProg, mapIndex') = convert_fexpr_to_unary fexpr (iProg, mapIndex, mapParams)
        iDef = (fname, iExpr)
        iProg' = (iDef : tempIProg)

convert_fexpr_to_unary :: FExpr -> TransformationUtilities -> (IExpr, IProgram, MapIndex)
convert_fexpr_to_unary (FVar tkn) (iprog, mapindex, mapP) =
        (IVar tkn, iprog, mapindex)
convert_fexpr_to_unary (FNum tkn) (iprog, mapindex, mapP) =
        (INum tkn, iprog, mapindex)
convert_fexpr_to_unary (FBool tkn) (iprog, mapindex, mapP) =
        (IBool tkn, iprog, mapindex)
convert_fexpr_to_unary (FBinaryOp op fexp1 fexp2) (iprog, mapindex, mapP) =
        (iexpFinal, iprog2, mapindex2)
    where
        (iexp1, iprog1, mapindex1) = convert_fexpr_to_unary fexp1 (iprog, mapindex, mapP)
        (iexp2, iprog2, mapindex2) = convert_fexpr_to_unary fexp2 (iprog1, mapindex1, mapP)
        iexpFinal = IBinaryOp op iexp1 iexp2
convert_fexpr_to_unary (FBooleanOp op fexp1 fexp2) (iprog, mapindex, mapP) =
        (iexpFinal, iprog2, mapindex2)
    where
        (iexp1, iprog1, mapindex1) = convert_fexpr_to_unary fexp1 (iprog, mapindex, mapP)
        (iexp2, iprog2, mapindex2) = convert_fexpr_to_unary fexp2 (iprog1, mapindex1, mapP)
        iexpFinal = IBooleanOp op iexp1 iexp2
convert_fexpr_to_unary (FCompOp op fexp1 fexp2) (iprog, mapindex, mapP) =
        (iexpFinal, iprog2, mapindex2)
    where
        (iexp1, iprog1, mapindex1) = convert_fexpr_to_unary fexp1 (iprog, mapindex, mapP)
        (iexp2, iprog2, mapindex2) = convert_fexpr_to_unary fexp2 (iprog1, mapindex1, mapP)
        iexpFinal = ICompOp op iexp1 iexp2
convert_fexpr_to_unary (FIfThenElse fexp1 fexp2 fexp3) (iprog, mapindex, mapP) =
        (iexpFinal, iprog3, mapindex3)
    where
        (iexp1, iprog1, mapindex1) = convert_fexpr_to_unary fexp1 (iprog, mapindex, mapP)
        (iexp2, iprog2, mapindex2) = convert_fexpr_to_unary fexp2 (iprog1, mapindex1, mapP)
        (iexp3, iprog3, mapindex3) = convert_fexpr_to_unary fexp3 (iprog2, mapindex2, mapP)
        iexpFinal = IIfThenElse iexp1 iexp2 iexp3
convert_fexpr_to_unary (FUnaryOp op fexp) (iprog, mapindex, mapP) =
        (iexpFinal, iprog1, mapindex1)
    where
        (iexp, iprog1, mapindex1) = convert_fexpr_to_unary fexp (iprog, mapindex, mapP)
        iexpFinal = IUnaryOp op iexp
convert_fexpr_to_unary (FParens fexp) (iprog, mapindex, mapP) =
        (iexpFinal, iprog1, mapindex1)
    where
        (iexp, iprog1, mapindex1) = convert_fexpr_to_unary fexp (iprog, mapindex, mapP)
        iexpFinal = IParens iexp
convert_fexpr_to_unary (FCall str fpar) (iprog, mapindex, mapP) =
        (iexpFinal, iprogFinal, mapindex2)
    where
        (findex, mapindex1) = find_and_update_index str mapindex
        iexpFinal = ICall findex str
        params = find_params str mapP
        (iprogFinal, mapindex2) = update_actuals fpar params (iprog, mapindex1, mapP)

update_actuals :: [FExpr] -> [String] -> TransformationUtilities -> (IProgram, MapIndex)
update_actuals [] [] (iprog, mapindex, _) = (iprog, mapindex)
update_actuals (fexpr : fs) (param : ps) (iprog, mapindex, mapP) = 
        (iprog2, mapindex1)
    where
        (iexpr, iprog1, mapindex1) = convert_fexpr_to_unary fexpr (iprog, mapindex, mapP)
        iprog2 = find_and_update_actuals param iexpr iprog1


find_and_update_index :: String -> MapIndex -> (Int, MapIndex)
find_and_update_index fn1 ((fn2, fi) : rs) = 
        if fn1 == fn2 then (fi, ((fn2, fi+1) : rs))
                      else (findex, ((fn2, fi) : res))
    where 
        (findex, res) = find_and_update_index fn1 rs

find_and_update_actuals :: String -> IExpr -> IProgram -> IProgram
find_and_update_actuals n1 iexpr ((n2, act) : rs) =
        if n1 == n2
            then
                case act of 
                    IActuals ls -> ((n1, IActuals (iexpr : ls)) : rs)
                    _ -> ((n2, act) : res)  -- Func def -> this code path will never be reached
            else 
                case act of 
                    IActuals ls -> ((n2, IActuals ls) : res)
                    _ -> ((n2, act) : res) -- This will be reached and it's needed for the case that we're checking a func def instead of a var def (with actuals list)
    where 
        res = find_and_update_actuals n1 iexpr rs

find_params :: String -> [(String, [String])] -> [String]
find_params p1 ((p2, x) : ps) = if p1 == p2 then x else find_params p1 ps   

-- // 2nd pass

-- 3rd pass (might be optional)
find_and_reverse_actuals :: IProgram -> IProgram
find_and_reverse_actuals [] = []
find_and_reverse_actuals ((n, act) : rs) =
    case act of 
        IActuals ls -> ((n, IActuals (reverse ls)) : res)
        _ -> ((n, act) : res)
    where 
        res = find_and_reverse_actuals rs
-- // 3rd pass



-- Transform logic
{- 
    1st pass:
    Parse FProgram to create actuals, aka IDefinitions with (name of variable, empty actuals list).
    Add them to a fresh IProgram.
    In this step, also create a mapping of (function name => index) for the call_X substitution,
    Optionally create a mapping of (function name => typical params).

    2nd pass:
    Parse FProgram to create the IDefinitions of the functions.
    We need to substitute each eg. f(1) with call_MAP[f] and add 1 to the x = actuals( ... ) list.

    - If we substitute a function call with call_X, then immediately parse the actuals list for subsequent 
    (nested) transformations: 

    f(f(1)) --> x = []
        a) call_0 f, x = [f(1)] 
        b) x = [call_1 f, 1]

    f(f(f(1))) --> x = []
        a) call_0 f, x = [f(f(1))] 
        b) call_0 f, x = [call_1 f, f(1)]
        c) call_0 f, x = [call_1 f, call_2 f, 1]

    3rd pass:

    Reverse the actuals lists
-}
