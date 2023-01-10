module Transform (transform) where

import Types

type MapParams = [(String, [String])]  -- Maps "function name" to "parameter array"
type MapIndex = [(String, Int)]        -- Maps "function name" to "current CALL index to be used"

type TransformationUtilities = ([IDefinition], [IDefinition], MapIndex, MapParams)

transform :: FProgram -> IProgram
transform fprog = iprog 
    where 
        util = add_actuals fprog ([], [], [], [])
        (iFunc, iAct, _, _) = convert_to_unary fprog util
        iAct' = find_and_reverse_actuals iAct
        iprog = iFunc ++ iAct'

-- 1st pass

-- Parse the FDefinitions and create empty IActuals for each unique parameter of the FProgram.
-- Side effect: Create "mapIndex" and "mapParams"
add_actuals :: FProgram -> TransformationUtilities -> TransformationUtilities
add_actuals (_, []) util = util
add_actuals (res, (fdef : fdefs)) (iFunc, iAct,  mapIndex,  mapParams) =
        add_actuals (res, fdefs)  (iFunc, iAct', mapIndex', mapParams')
    where
        (fname, fparams, fexpr) = fdef
        iAct' = parse_def_for_actuals fparams iAct
        mapIndex' = ((fname, 0) : mapIndex)
        mapParams' = ((fname, fparams) : mapParams)

-- Parse the "Parameter" field of an FDefinition and create empty IActuals for each param.
parse_def_for_actuals :: [String] -> [IDefinition] -> [IDefinition]
parse_def_for_actuals [] iAct = iAct
parse_def_for_actuals (param : rest) iAct = ( (param, IActuals []) : iAct' )
    where
        iAct' = parse_def_for_actuals rest iAct

-- // 1st pass

-- 2nd pass

-- (wrapper) Combine "result" with the rest of "FDefinitions" and transform it to an IProgram
convert_to_unary :: FProgram -> TransformationUtilities -> TransformationUtilities
convert_to_unary (res, fdefs) util =
        convert_fdefs_to_unary totalFdefs util
    where
        totalFdefs = (("result", [], res) : fdefs)

-- Convert FDefinitions to IDefinitions
convert_fdefs_to_unary :: [FDefinition] -> TransformationUtilities -> TransformationUtilities
convert_fdefs_to_unary [] util = util
convert_fdefs_to_unary ((fname, _, fexpr) : rs) util = 
        convert_fdefs_to_unary rs (iFunc', iAct', mapIndex', mapParams)
    where
        (iFunc, iAct, mapIndex, mapParams) = util
        (iExpr, (_, iAct', mapIndex', _)) = convert_fexpr_to_unary fexpr util
        iDef = (fname, iExpr)
        iFunc' = (iDef : iFunc)

-- Convert FExpr to IExpr (updating the "actuals" lists if needed)
convert_fexpr_to_unary :: FExpr -> TransformationUtilities -> (IExpr, TransformationUtilities)
convert_fexpr_to_unary (FVar tkn) util = -- util = (iFunc, iAct, mapIndex, mapP)
        (IVar tkn, util)
convert_fexpr_to_unary (FNum tkn) util =
        (INum tkn, util)
convert_fexpr_to_unary (FBool tkn) util =
        (IBool tkn, util)
convert_fexpr_to_unary (FBinaryOp op fexp1 fexp2) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct2, mapIndex2, mapP))
    where
        (iexp1, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexp1 (iFunc, iAct, mapIndex, mapP)
        (iexp2, (_, iAct2, mapIndex2, _)) = convert_fexpr_to_unary fexp2 (iFunc, iAct1, mapIndex1, mapP)
        iexpFinal = IBinaryOp op iexp1 iexp2
convert_fexpr_to_unary (FBooleanOp op fexp1 fexp2) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct2, mapIndex2, mapP))
    where
        (iexp1, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexp1 (iFunc, iAct, mapIndex, mapP)
        (iexp2, (_, iAct2, mapIndex2, _)) = convert_fexpr_to_unary fexp2 (iFunc, iAct1, mapIndex1, mapP)
        iexpFinal = IBooleanOp op iexp1 iexp2
convert_fexpr_to_unary (FCompOp op fexp1 fexp2) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct2, mapIndex2, mapP))
    where
        (iexp1, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexp1 (iFunc, iAct, mapIndex, mapP)
        (iexp2, (_, iAct2, mapIndex2, _)) = convert_fexpr_to_unary fexp2 (iFunc, iAct1, mapIndex1, mapP)
        iexpFinal = ICompOp op iexp1 iexp2
convert_fexpr_to_unary (FIfThenElse fexp1 fexp2 fexp3) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct3, mapIndex3, mapP))
    where
        (iexp1, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexp1 (iFunc, iAct, mapIndex, mapP)
        (iexp2, (_, iAct2, mapIndex2, _)) = convert_fexpr_to_unary fexp2 (iFunc, iAct1, mapIndex1, mapP)
        (iexp3, (_, iAct3, mapIndex3, _)) = convert_fexpr_to_unary fexp3 (iFunc, iAct2, mapIndex2, mapP)
        iexpFinal = IIfThenElse iexp1 iexp2 iexp3
convert_fexpr_to_unary (FUnaryOp op fexp) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct1, mapIndex1, mapP))
    where
        (iexp, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexp (iFunc, iAct, mapIndex, mapP)
        iexpFinal = IUnaryOp op iexp
convert_fexpr_to_unary (FParens fexp) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct1, mapIndex1, mapP))
    where
        (iexp, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexp (iFunc, iAct, mapIndex, mapP)
        iexpFinal = IParens iexp
convert_fexpr_to_unary (FCall str fpar) (iFunc, iAct, mapIndex, mapP) =
        (iexpFinal, (iFunc, iAct1, mapIndex2, mapP))
    where
        (findex, mapIndex1) = find_and_update_index str mapIndex
        iexpFinal = ICall findex str
        params = find_params str mapP
        (_, iAct1, mapIndex2, _) = update_actuals fpar params (iFunc, iAct, mapIndex1, mapP)

update_actuals :: [FExpr] -> [String] -> TransformationUtilities -> TransformationUtilities
update_actuals [] [] util = util
update_actuals (fexpr : fs) (param : ps) (iFunc, iAct, mapIndex, mapP) = 
        update_actuals fs ps (iFunc, iAct2, mapIndex1, mapP)
    where
        previous_actuals_len = length (find_actuals param iAct)
        (iexpr, (_, iAct1, mapIndex1, _)) = convert_fexpr_to_unary fexpr (iFunc, iAct, mapIndex, mapP)
        new_actuals_len = length (find_actuals param iAct1)
        index_to_insert_iexpr = new_actuals_len - previous_actuals_len
        iAct2 = find_and_update_actuals param index_to_insert_iexpr iexpr iAct1

find_and_update_index :: String -> MapIndex -> (Int, MapIndex)
find_and_update_index fn1 ((fn2, fi) : rs) = 
        if fn1 == fn2 then (fi, ((fn2, fi+1) : rs)) else (findex, ((fn2, fi) : res))
    where 
        (findex, res) = find_and_update_index fn1 rs

find_and_update_actuals :: String -> Int -> IExpr -> [IDefinition] -> [IDefinition]
find_and_update_actuals n1 index iexpr ((n2, act) : rs) =
        if n1 == n2 then case act of 
                            IActuals ls -> ((n1, IActuals (insert_expr_in_actuals iexpr ls index)) : rs)
                    else 
                        ((n2, act) : res)
    where 
        res = find_and_update_actuals n1 index iexpr rs

find_actuals :: String -> [IDefinition] -> [IExpr]
find_actuals n1 ((n2, act) : rs) =
    if n1 == n2 then case act of IActuals ls -> ls
                else find_actuals n1 rs

find_params :: String -> [(String, [String])] -> [String]
find_params p1 ((p2, x) : ps) = if p1 == p2 then x else find_params p1 ps   

insert_expr_in_actuals :: IExpr -> [IExpr] -> Int -> [IExpr]
insert_expr_in_actuals iexpr actuals 0 = (iexpr : actuals)
insert_expr_in_actuals iexpr (actual : rs) index =
    actual : (insert_expr_in_actuals iexpr rs (index - 1))

-- // 2nd pass

-- 3rd pass
find_and_reverse_actuals :: [IDefinition] -> [IDefinition]
find_and_reverse_actuals [] = []
find_and_reverse_actuals ((n, act) : rs) =
    case act of IActuals ls -> ((n, IActuals (reverse ls)) : res)
    where 
        res = find_and_reverse_actuals rs
-- // 3rd pass
