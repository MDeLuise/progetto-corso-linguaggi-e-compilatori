module TypeChecking where

import LexGrammar
import ParGrammar
import SkelGrammar
import AbsGrammar
import Type

import ErrM
import Data.Map
import Data.Either
import Data.Maybe
import qualified Data.Set as Set
  

data TypeCheckRes
    = TypeInfo {getEnv:: Env, getType:: Type, getPos:: LexGrammar.Posn, getSize:: Integer, getId:: String, getLocal:: Int}
    | Errs {getErrs:: [String]}
    deriving (Show)


data Param = Param {getParamType:: Type, getParamPos:: LexGrammar.Posn, getParamModality:: AbsGrammar.Modality, getParamString:: String, getParamSize:: Integer}
  deriving (Eq, Ord, Show, Read)


type Env = Map String [EnvEntry]


data EnvEntry 
    = Var {varPos:: LexGrammar.Posn, varType:: Type, varEditable:: Bool, varSize:: Integer, varMod:: AbsGrammar.Modality, varLineMod:: Int, varCheck:: Bool}
    | Fun {funPos:: LexGrammar.Posn, funParams:: [Param], funType:: Type, funSize:: Integer}
    deriving (Show)



-- Size of the SympType
dimInt = 4
dimChar = 1
dimBool = 1
dimVoid = 0
dimPointer = 8
dimFloat64 = 8


firstEl a b = a
mergeEls a b = a ++ b
replaceFirst a b = a ++ (tail b)



-- Takes 2 types and returns true if the first one is a subtype of the second one, false otherwise
subType :: Type -> Type -> Bool
subType Bottom _ = True
subType (SimpTyp T_Int) (SimpTyp T_Float64) = True
subType x y = x == y


-- Takes 2 types and returns true if the types are compatible, false otherwise
compatibleType :: Type -> Type -> Bool
compatibleType Bottom _ = True
compatibleType (Type.Array t1 s1 _) (Type.Array t2 s2 _) = (s2 == s1) && (compatibleType t1 t2)
compatibleType (Type.Array _ _ _) _ = False
compatibleType _ (Type.Array _ _ _) = False
compatibleType t1 t2 = subType t1 t2


-- returns a TypeCheckRes for that VarDeclInit or a list of Errors if present
getTypeFromVarDeclInit :: AbsGrammar.VarDeclInit LexGrammar.Posn -> Env -> Posn -> TypeCheckRes
getTypeFromVarDeclInit decl env pos = case decl of
  
  ConDeclIn p _ (Ident pi str) typ rexpr check ->
    if containVoidType typ
      then Errs ["cannot use void as a type at " ++ (show p)] 
      else
        case (getTypeFromComplexRExpr rexpr env, getTypeFromTypeSpec typ env (getMaybeTypeFromComplexRExpr rexpr env)) of
          (TypeInfo _ te pe _ _ le, TypeInfo _ tt _ s _ _) ->
            if (not (typeIsArray te)) && check
              then
                Errs ["only arrays can have checked bounds at " ++ (show p)]
              else
                if compatibleType te tt
                  then case Data.Map.lookup str env of
                    Nothing -> TypeInfo (insertWith firstEl str ((Var pi tt False s Modality1 (getPosRow pos) check):[]) env) tt p s "" 0
                    Just entries ->
                      if (and $ Prelude.map (canOverride pos) entries)
                        then TypeInfo (insertWith mergeEls str ((Var pi tt False s Modality1 (getPosRow pos) check):[]) env) tt p s "" 0
                        else Errs ["cannot redefine " ++ str ++ " at " ++ (show pi) ++ ", it's already defined"]
                  else Errs ["type " ++ (show te) ++ " not compatible with type " ++ (show tt) ++ " at " ++ (show pe)]
          (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
          (Errs strs, _) -> Errs strs
          (_, Errs strs) -> Errs strs
  
  VarDeclIn p _ (Ident pi str) typ rexpr check ->
    if containVoidType typ
      then Errs ["cannot use void as a type at " ++ (show p)]
      else
        case (getTypeFromComplexRExpr rexpr env, getTypeFromTypeSpec typ env (getMaybeTypeFromComplexRExpr rexpr env)) of
          (TypeInfo _ te pe _ _ _, TypeInfo _ tt _ s _ _) ->
            if (not (typeIsArray te)) && check
              then
                Errs ["only arrays can have checked bounds at " ++ (show p)]
              else
                if compatibleType te tt
                  then case Data.Map.lookup str env of
                    Nothing -> TypeInfo (insertWith firstEl str ((Var pi tt True s Modality1 (getPosRow pos) check):[]) env) tt p s "" 0
                    Just entries -> if (and $ Prelude.map (canOverride pos) entries)
                      then TypeInfo (insertWith mergeEls str ((Var pi tt True s Modality1 (getPosRow pos) check):[]) env) tt p s "" 0
                      else Errs ["cannot redefine " ++ str ++ " at " ++ (show pi) ++ ", it's already defined"]
                  else Errs ["type " ++ (show te) ++ " not compatible with type " ++ (show tt) ++ " at " ++ (show pe)]
          (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
          (Errs strs, _) -> Errs strs
          (_, Errs strs) -> Errs strs


pickMax :: Int -> Int -> Int
pickMax a b = if a > b then a else b


typeIsArray :: Type -> Bool
typeIsArray (Type.Array _ _ _) = True
typeIsArray _ = False


getMaybeTypeFromComplexRExpr :: ComplexRExpr Posn -> Env -> Maybe Type
getMaybeTypeFromComplexRExpr comp env = case (getTypeFromComplexRExpr comp env) of
  TypeInfo _ tc _ _ _ _-> Just tc
  Errs _ -> Nothing


canOverride :: Posn -> EnvEntry -> Bool
canOverride blockPos entry = case entry of
  Var varPos _ _ _ _ _ _ -> (getPosRow blockPos) > (getPosRow varPos)
  _ -> False


-- Given a TypeSpec, returns true if it contains the type void, false otherwise
containVoidType:: (TypeSpec Posn) -> Bool
containVoidType (BasTyp _ (BasicType_void _)) = True
containVoidType (BasTyp _ _) = False
containVoidType ty = containVoidType $ typeSpecTypeSpec ty


-- Given a TypeSpec and an environment returns a TypeCheckRes for that TypeSpec or a list of Errors if present
getTypeFromTypeSpec :: TypeSpec LexGrammar.Posn -> Env -> Maybe Type -> TypeCheckRes
getTypeFromTypeSpec typ env arr = case typ of
  ArrUnDef pos ty -> case arr of
    Just (Type.Array te len se) -> case (getTypeFromTypeSpec ty env (Just te)) of
      TypeInfo et tt _ st _ _ -> TypeInfo et (Type.Array tt len st) pos (len * st) "" 0
      Errs strs -> Errs strs
    _ -> Errs ["could not determine array size at " ++ show(pos)]

  BasTyp pos bas -> case bas of
    BasicType_boolean _ -> TypeInfo env (SimpTyp T_Boolean) pos dimBool "" 0
    BasicType_char _ -> TypeInfo env (SimpTyp T_Char) pos dimChar "" 0
    BasicType_float64 _ -> TypeInfo env (SimpTyp T_Float64) pos dimFloat64 "" 0
    BasicType_int _ -> TypeInfo env (SimpTyp T_Int) pos dimInt "" 0
    BasicType_void _ -> TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0
    BasicType_String _ -> TypeInfo env (SimpTyp T_String) pos dimPointer "" 0

  Pointer pos ty -> case arr of
    Just (Point tp _) -> case (getTypeFromTypeSpec ty env (Just tp)) of
      TypeInfo et tt _ st _ _ -> TypeInfo et (Point tt st) pos dimPointer "" 0
      Errs strs -> Errs strs
    _ -> case (getTypeFromTypeSpec ty env Nothing) of
      TypeInfo et tt _ st _ _ -> TypeInfo et (Point tt st) pos dimPointer "" 0
      Errs strs -> Errs strs

  ArrDef pos ty dim -> case arr of
    Just (Type.Array te len se) -> case (getTypeFromTypeSpec ty env (Just te)) of
      TypeInfo et tt _ st _ _ -> TypeInfo et (Type.Array tt (myIntegerInt dim) st) pos ((myIntegerInt dim) * st) "" 0
      Errs strs -> Errs strs
    _ -> case (getTypeFromTypeSpec ty env Nothing) of
      TypeInfo et tt _ st _ _ -> TypeInfo et (Type.Array tt (myIntegerInt dim) st) pos ((myIntegerInt dim) * st) "" 0
      Errs strs -> Errs strs


-- Given a ComplexRExpr and an environment returns a TypeCheckRes for that ComplexRExpr or a list of Errors if present
getTypeFromComplexRExpr :: ComplexRExpr LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromComplexRExpr comp env = case comp of
  AbsGrammar.Array pos _ exprs | containsAnArrayVar exprs env -> Errs ["cannot assign an array to a variable at " ++ (show $ pos)]
  Simple pos _ rexpr -> getTypeFromRExpr rexpr env
  AbsGrammar.Array pos _ rexprs ->
    let inftyp = getMostGeneralType $ Prelude.map (\x -> getTypeFromComplexRExpr x env) rexprs in
        case inftyp of
            Errs _ -> inftyp
            _ -> TypeInfo env (Type.Array (getType inftyp) (toInteger $ length rexprs) (getSize inftyp)) pos (getSize inftyp) "" 0


containsAnArrayVar:: [(ComplexRExpr Posn)] -> Env -> Bool
containsAnArrayVar [] _ = False
containsAnArrayVar exprs env = any (\x -> containsAnArrayVarAux x env) exprs


containsAnArrayVarAux:: (ComplexRExpr Posn) -> Env -> Bool
containsAnArrayVarAux (Simple _ _ ty) env = case ty of
  expr@(IfRe _ _ _ _ _) -> case getTypeFromRExpr expr env of
    Errs _ -> False
    _ -> case getType $ getTypeFromRExpr expr env of
      Type.Array _ _ _ -> True
      _ -> False
  expr@(Lexpr _ _ _) -> case getTypeFromRExpr expr env of
    Errs _ -> False
    _ -> case getType $ getTypeFromRExpr expr env of
      Type.Array _ _ _ -> True
      _ -> False
  _ -> False

containsAnArrayVarAux _ _ = False


isArrayType:: RExpr Posn -> Env -> Bool
isArrayType (Lexpr _ _ (BasLExpr _ _ (Id _ _ _ id))) env =
  case Data.Map.lookup id env of
    Just (((Var _ (Type.Array _ _ _) _ _ _ _ _)):xs) -> True
    Just _ -> False
    Nothing -> False -- case of undeclared variable, the error will raise not here
isArrayType _ _ = False


-- Given a RExpr and an environment returns a TypeCheckRes for that ComplexRExpr or a list of Errors if present
getTypeFromRExpr :: RExpr LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromRExpr rexpr env = case rexpr of
    
    AbsGrammar.Int pos _ _ -> TypeInfo env (SimpTyp T_Int) pos dimInt "" 0
    AbsGrammar.Char pos _ _ -> TypeInfo env (SimpTyp T_Char) pos dimChar "" 0
    AbsGrammar.String pos _ _ -> TypeInfo env (SimpTyp T_String) pos dimPointer "" 0
    AbsGrammar.Float pos _ _ -> TypeInfo env (SimpTyp T_Float64) pos dimFloat64 "" 0
    AbsGrammar.Bool pos _ _ -> TypeInfo env (SimpTyp T_Boolean) pos dimBool "" 0
    AbsGrammar.Ref pos _ lexpr -> case (getTypeFromLExpr lexpr env) of
      TypeInfo _ tl _ sl _ ll -> TypeInfo env (Point tl sl) pos dimPointer "" ll
      Errs strs -> Errs strs
    
    AbsGrammar.BoolBinOp pos _ _ lexpr rexpr ->
      case (getTypeFromRExpr lexpr env, getTypeFromRExpr rexpr env) of
        (TypeInfo _ tl pl _ _ _, TypeInfo _ tr pr _ _ _) -> case (subType tl (SimpTyp T_Boolean), subType tr (SimpTyp T_Boolean)) of
          (True, True) -> TypeInfo env (SimpTyp T_Boolean) pl dimBool "" 0
          (True, False) -> Errs ["not subtype of boolean at pos " ++ (show pr)]
          (False, True) -> Errs ["not subtype of boolean at pos " ++ (show pl)]
          (False, False) -> Errs (("not subtype of boolean at pos " ++ (show pl)):("not subtype of boolean at pos " ++ (show pr)):[])
        (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
        (Errs strs, _) -> Errs strs
        (_, Errs strs) -> Errs strs
    
    AbsGrammar.Not pos _ rexpr -> case getTypeFromRExpr rexpr env of
      TypeInfo _ te pe _ _ _ ->
        if subType te (SimpTyp T_Boolean)
          then TypeInfo env (SimpTyp T_Boolean) pos dimBool "" 0
          else Errs ["not subtype of boolean at pos " ++ (show pe)]
      Errs strs -> Errs strs
    
    AbsGrammar.BinOp pos _ _ expl expr -> case (getTypeFromRExpr expl env, getTypeFromRExpr expr env) of
      (TypeInfo _ tl pl dl _ _, TypeInfo _ tr pr dr _ _) -> case (subType tl (SimpTyp T_Float64), subType tr (SimpTyp T_Float64)) of
        (True, True) -> if subType tr tl then TypeInfo env tl pl dl "" 0 else TypeInfo env tr pl dr "" 0
        (True, False) -> Errs ["not numeric type at pos " ++ (show pr)]
        (False, True) -> Errs ["not numeric type at pos " ++ (show pl)]
        (False, False) -> Errs (("not numeric type at pos " ++ (show pl)):("not numeric type at pos " ++ (show pr)):[])
      (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
      (Errs strs, _) -> Errs strs
      (_, Errs strs) -> Errs strs

    AbsGrammar.Comparison pos _ eqOp fstOpe sndOpe -> case (getTypeFromRExpr fstOpe env, getTypeFromRExpr sndOpe env) of
      (TypeInfo _ ty1 ps1 _ _ _, TypeInfo _ ty2 ps2 _ _ _) ->
        case (ty1,ty2) of
          (Type.Array _ _ _, Type.Array _ _ _) ->Errs["cannot use comparison with array at " ++ (show pos)]
          (Type.Array _ _ _, _) ->Errs["cannot use comparison with array at " ++ (show pos)] 
          (_, Type.Array _ _ _) ->Errs["cannot use comparison with array at " ++ (show pos)]
          (Type.Point _ _, Type.Point _ _) ->Errs["cannot use comparison with pointer at " ++ (show pos)]
          (Type.Point _ _, _) ->Errs["cannot use comparison with pointer at " ++ (show pos)] 
          (_, Type.Point _ _) ->Errs["cannot use comparison with pointer at " ++ (show pos)]
          _ ->
            if subType ty1 ty2 || subType ty2 ty1
              then TypeInfo env (SimpTyp T_Boolean) pos dimBool "" 0
              else Errs ["type " ++ (show ty1) ++ " in " ++ (show ps1) ++ " not compatible with type " ++ (show ty2) ++ " in " ++ (show ps2)]
      (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
      (Errs strs, _) -> Errs strs
      (_, Errs strs) -> Errs strs

    AbsGrammar.Neg pos _ rexpr -> case getTypeFromRExpr rexpr env of
      TypeInfo _ te pe de _ _ ->
        if subType te (SimpTyp T_Float64)
          then TypeInfo env te pos de "" 0
          else Errs["not numeric type at pos " ++ (show pe)]
      Errs strs -> Errs strs

    AbsGrammar.Equality pos _ eqOp fstOpe sndOpe -> case (getTypeFromRExpr fstOpe env, getTypeFromRExpr sndOpe env) of
      (TypeInfo _ ty1 ps1 _ _ _, TypeInfo _ ty2 ps2 _ _ _) ->
        case (ty1,ty2) of
          (Type.Array _ _ _, Type.Array _ _ _) ->Errs["cannot use equality with array at " ++ (show pos)]
          (Type.Array _ _ _, _) ->Errs["cannot use equality with array at " ++ (show pos)] 
          (_, Type.Array _ _ _) ->Errs["cannot use equality with array at " ++ (show pos)]
          _ ->
            if subType ty1 ty2 || subType ty2 ty1
              then TypeInfo env (SimpTyp T_Boolean) pos dimBool "" 0
              else Errs["type " ++ (show ty1) ++ " in " ++ (show ps1) ++ " not compatible with type " ++ (show ty2) ++ " in " ++ (show ps2)]
      (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
      (Errs strs, _) -> Errs strs
      (_, Errs strs) -> Errs strs

    AbsGrammar.Lexpr pos _ expr -> getTypeFromLExpr expr env

    AbsGrammar.FCall pos _ id params -> getTypeFromFCall pos (identString id) params env
    
    AbsGrammar.IfRe pos _ guard the els -> case (getTypeFromRExpr guard env, getTypeFromRExpr the env, getTypeFromRExpr els env) of
      (TypeInfo _ tg pg _ _ _, TypeInfo _ tt pt st _ _,  TypeInfo _ te pe se _ _) -> case (subType tg (SimpTyp T_Boolean), (subType tt te || subType te tt)) of
               (True, True) -> 
                 let su = if (subType tt te) then se else st in
                 let tu = if (subType tt te) then te else tt in
                   TypeInfo env tu pos su "" 0
               (False, True) -> Errs ["type " ++ (show tg) ++ " in " ++ (show pg) ++ " not compatible with expected type boolean"]
               (True, False) -> Errs ["type " ++ (show tt) ++ " in " ++ (show pt) ++ " not compatible with type" ++ (show te) ++ " in " ++ (show pe)]
      (Errs strs1, Errs strs2, Errs strs3) -> Errs (strs1 ++ strs2 ++ strs3)
      (Errs strs1, Errs strs2, _) -> Errs (strs1 ++ strs2)
      (Errs strs1, _, Errs strs3) -> Errs (strs1 ++ strs3)
      (_, Errs strs2, Errs strs3) -> Errs (strs2 ++ strs3)
      (_, _, Errs strs3) -> Errs strs3
      (_, Errs strs2, _) -> Errs strs2
      (Errs strs1, _, _) -> Errs strs1


checkParameters :: Parameter LexGrammar.Posn -> Either TypeChecking.Param [String]
checkParameters (AbsGrammar.Param pos _ Modality_valres _ (ArrDef _ _ _)) = Right ["array cannot be used in valres modality at " ++ (show pos)]
checkParameters (AbsGrammar.Param pos _ Modality_valres _ (ArrUnDef _ _)) = Right ["array cannot be used in valres modality at " ++ (show pos)]
checkParameters (AbsGrammar.Param pos _ mod (Ident p str) pt) = case getTypeFromTypeSpec pt empty Nothing of
  TypeInfo _ typ _ s _ _ -> Left (TypeChecking.Param typ pos mod str s)
  Errs strs -> Right strs


parInEnv :: Env -> Either TypeChecking.Param [String] -> Env
parInEnv env (Right _) = env
parInEnv env (Left par) = case par of
  TypeChecking.Param tp pos Modality_const str s -> insertWith firstEl str ((Var pos tp False s Modality_const 0 True):[]) env
  TypeChecking.Param tp pos mod str s -> insertWith firstEl str ((Var pos tp True s mod 0 True):[]) env


addParameters :: [AbsGrammar.Parameter LexGrammar.Posn] -> Env -> Env
addParameters pars env = Prelude.foldl parInEnv env $ Prelude.map checkParameters pars


conflictParameters :: [TypeChecking.Param] -> [TypeChecking.Param] -> Bool
conflictParameters [] [] = True
conflictParameters ((TypeChecking.Param Bottom _ _ _ _):xs) ys = True
conflictParameters pars ((TypeChecking.Param Bottom _ _ _ _):ys) = True
conflictParameters pars [] = False
conflictParameters [] pars = False
conflictParameters (x:xs) (y:ys) =
  let tx = getParamType x in
  let ty = getParamType y in
    ((subType tx ty) || (subType ty tx)) && conflictParameters xs ys


-- Given an EnvEntry returns a list of parameters
getEnvEntryPars :: EnvEntry -> [TypeChecking.Param]
getEnvEntryPars (Fun _ pars _ _) = pars
getEnvEntryPars (Var p _ _ _ _ _ _) = (TypeChecking.Param Bottom p Modality1 "variable" 0):[]


-- returns the TypeCheckRes of that Stmt or a list of Errors if present
getTypeFromStmt :: Stmt LexGrammar.Posn -> Env -> Posn -> TypeCheckRes
getTypeFromStmt rexpr env pos = case rexpr of
 
  AbsGrammar.VarDec p ss -> case getTypeFromVarDeclInit ss env pos of
    TypeInfo  en _ _ _ _ _ -> TypeInfo en (SimpTyp T_Void) p dimVoid "" 0
    Errs strs -> Errs strs

  AbsGrammar.Sel p sel -> getTypeFromSelectionStmt sel env

  AbsGrammar.While p guard block -> case (getTypeFromRExpr guard env, getTypeFromBlockDecl block $ insertWith firstEl "while" [Var (Pn 0 1 1) Bottom True 0 Modality1 0 False] env) of
    (TypeInfo _ gTy gPs _ _ _, TypeInfo _ sTy sPs _ _ _) -> case (subType gTy (SimpTyp T_Boolean), subType sTy (SimpTyp T_Void)) of
      (True, True) -> TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
      (False, False) -> Errs (["type " ++ (show gTy) ++ " in " ++ (show gPs) ++ " not compatible with expected type boolean"] ++ [" type " ++ (show sTy) ++ " in " ++ (show sPs) ++ " not compatible with expected type void"])
      (True, False) -> Errs (["type " ++ (show sTy) ++ " in " ++ (show sPs) ++ " not compatible with expected type void"])
      (False, True) -> Errs (["type " ++ (show gTy) ++ " in " ++ (show gPs) ++ " not compatible with expected type boolean"])
    (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
    (Errs strs, _) -> Errs strs
    (_, Errs strs) -> Errs strs

  AbsGrammar.DoWhile p guard block -> case (getTypeFromRExpr guard env, getTypeFromBlockDecl block $ insertWith firstEl "while" [Var (Pn 0 1 1) Bottom True 0 Modality1 0 False] env) of
    (TypeInfo _ gTy gPs _ _ _, TypeInfo _ sTy sPs _ _ _) -> case (subType gTy (SimpTyp T_Boolean), subType sTy (SimpTyp T_Void)) of
      (True, True) -> TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
      (False, False) -> Errs (["type " ++ (show gTy) ++ " in " ++ (show gPs) ++ " not compatible with expected type boolean"] ++ [" type " ++ (show sTy) ++ " in " ++ (show sPs) ++ " not compatible with expected type void"])
      (True, False) -> Errs (["type " ++ (show sTy) ++ " in " ++ (show sPs) ++ " not compatible with expected type void"])
      (False, True) -> Errs (["type " ++ (show gTy) ++ " in " ++ (show gPs) ++ " not compatible with expected type boolean"])
    (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
    (Errs strs, _) -> Errs strs
    (_, Errs strs) -> Errs strs

  AbsGrammar.For p loopVar st fi block -> case (getTypeFromRExpr st env, getTypeFromRExpr fi env) of
    (TypeInfo _ (SimpTyp T_Int) _ _ _ _, TypeInfo _ (SimpTyp T_Int) _ _ _ _) ->
      let newEnv = insertWith mergeEls "while" [Var (Pn 0 1 1) Bottom False 0 Modality1 0 False] env in
      let forEnv = insertWith mergeEls (identString loopVar) [Var (identContent loopVar) (SimpTyp T_Int) False dimInt Modality1 0 False] newEnv in
        case getTypeFromBlockDecl block forEnv of
          Errs str -> Errs str
          _ -> TypeInfo forEnv (SimpTyp T_Void) p dimVoid "" 0
    (TypeInfo _ _ _ _ _ _, TypeInfo _ _ _ _ _ _) -> Errs ["for range must be integer type at " ++ show p]
    (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
    (Errs strs, _) -> Errs strs
    (_, Errs strs) -> Errs strs

  AbsGrammar.Switch p exp block -> case getTypeFromRExpr exp env of
    Errs strs -> Errs strs
    TypeInfo _ (Type.Array _ _ _) posExp  _ _ _ -> Errs ["cannot use array in switch expression at " ++ show posExp]
    _ ->
      let blockFoo = getTypeFromSwitchBlock block (getType $ getTypeFromRExpr exp env) env in
      case blockFoo of
        Errs strs -> Errs strs
        _ -> TypeInfo env (SimpTyp T_Void) p dimVoid "" 0 
    
  AbsGrammar.LExprStmt _ lexp -> case getTypeFromLExpr lexp env of
    TypeInfo _ _ p _ _ _ -> TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
    Errs strs -> Errs strs

  AbsGrammar.Assgn p sx op dx -> 
    case sx of
      IncDec po _ _ _ -> Errs ["overriding inc/dec operation with assignment at " ++ (show po)]
      _ -> case (getTypeFromLExpr sx env, getTypeFromRExpr dx env) of
        (TypeInfo _ typ1 pos1 size1 id _, TypeInfo _ typ2 pos2 size2 _ modl) -> case Data.Map.lookup id env of
            Just entries ->
              let entry = (head entries) in
                case head entries of
                  Var _ _ True _ _ envl _ ->
                    let newEnv = (insertWith replaceFirst id [(entry {varLineMod = (pickMax envl modl)})] env) in
                      if subType typ2 typ1
                        then case getAssgnType op of
                          'n' -> case typ1 of
                            (SimpTyp T_Char) -> Errs ["type " ++ (show (SimpTyp T_Int)) ++ " in " ++ (show pos2) ++ " not compatible with type " ++ (show typ1) ++ " in " ++ (show pos1)]
                            (SimpTyp T_Int) -> TypeInfo newEnv (SimpTyp T_Void) p dimVoid "" 0
                            (SimpTyp T_Float64) -> TypeInfo newEnv (SimpTyp T_Void) p dimVoid "" 0
                            _ -> Errs ["type " ++ (show typ1) ++ " of L-Expr at " ++ (show pos1) ++ " is not numeric"]
                          'b' -> case (subType typ1 (SimpTyp T_Boolean)) of
                            True -> TypeInfo newEnv (SimpTyp T_Void) p dimVoid "" 0
                            False -> Errs ["type " ++ (show typ1) ++ " of L-Expr at " ++ (show pos1) ++ " is not compatible with boolean"]
                          'o' -> TypeInfo newEnv (SimpTyp T_Void) p dimVoid "" 0
                        else  Errs ["type " ++ (show typ2) ++ " in " ++ (show pos2) ++ " not compatible with type " ++ (show typ1) ++ " in " ++ (show pos1)]
                  Var _ _ False _ _ _ _ -> Errs ["cannot assign a value to a constant at " ++ show p]
                  Fun fpos _ _ _ -> Errs ["cannot use function identifiers in L-expressions at " ++ show fpos]
            Nothing -> Errs ["variable undeclared at " ++ show p]
        (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
        (Errs strs, _) -> Errs strs
        (_, Errs strs) -> Errs strs

  -- for functions with 0 parameters
  AbsGrammar.FunDec p (Ident pi id) [] retyp block -> case Data.Map.lookup id env of
    Just _ -> case getTypeFromTypeSpec retyp empty Nothing of
      TypeInfo _ tre pre dre _ _ -> case tre of
        Type.Array _ _ _ -> Errs ["functions cannot return arrays"]
        _ -> case (getTypeFromBlockDecl block (insertWith firstEl "return" ((Var pre tre False dre Modality1 (getPosRow pos) False):[]) env)) of
          TypeInfo _ tb _ _ _ _ -> if subType tb (SimpTyp T_Void)
            then if tre == (SimpTyp T_Void)
                  then TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
                  else if hasReturn block
                    then TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
                    else Errs ["function " ++ id ++ " does not have a return statement"]
            else Errs ["internal error (error code: 0)"]
          Errs strs -> Errs strs
      Errs strs -> Errs strs
    Nothing -> Errs ["internal error (error code: 1)"]

  -- for functions with > 0 parameters
  AbsGrammar.FunDec p (Ident pi id) pars retyp block -> case Data.Map.lookup id env of
    Just _ -> case getTypeFromTypeSpec retyp empty Nothing of
      TypeInfo _ tre pre dre _ _ -> case tre of
        Type.Array _ _ _ -> Errs ["functions cannot return arrays"]
        _ ->
          if hasDuplicatesParams pars
            then Errs ["cannot use different parameters with the same formal name at " ++ show p]
            else
              let checked = Prelude.map checkParameters pars in
                case getTypeFromBlockDecl block $ insertWith firstEl "return" ((Var pre tre False dre Modality1 (getPosRow pos) False):[]) (addParameters pars env) of
                  TypeInfo _ tb _ _ _ _ -> if subType tb (SimpTyp T_Void)
                    then case (concat $ Data.Either.rights checked) of
                      [] -> if tre == (SimpTyp T_Void)
                        then TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
                        else if hasReturn block
                          then TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
                          else Errs ["function " ++ id ++ " does not have a return statement"]
                      errs -> Errs errs
                    else Errs ["internal error (error code: 2)"]
                  Errs strs -> Errs strs
      Errs strs -> Errs strs
    Nothing -> Errs ["declaration of function " ++ id ++ " at position " ++ (show p) ++ " failed"]

  AbsGrammar.Jmp _ rExpr -> getTypeFromJumpStmt rExpr env

  AbsGrammar.ProcCall p id params -> getTypeFromFCall p (identString id) params env

  AbsGrammar.Comp p block -> getTypeFromBlockDecl block env


getTypeFromSwitchBlock:: (SwitchBlock Posn) -> Type -> Env -> TypeCheckRes
getTypeFromSwitchBlock (BlockSwitch p []) _ _ = Errs ["switch statement must have at least one default case at " ++ show p]
getTypeFromSwitchBlock (BlockSwitch p xs) ty env = 
  if not $ oneDefault xs
    then Errs ["switch statement must have one default case and it must be the last one at " ++ show p]
    else
      let foo = Prelude.map (\x -> getTypeFromSwitchMatch x ty env) xs in
        case (onlyErrors foo) of
          [] -> TypeInfo env (SimpTyp T_Void) p dimVoid "" 0
          errList -> Prelude.foldl (\x acc -> (Errs ((getErrs acc) ++ (getErrs x)))) (Errs []) errList


oneDefault:: [SwitchMatch Posn] -> Bool
oneDefault [] = False
oneDefault ((Default _ _):[]) = True
oneDefault ((Default _ _):xs) = False
oneDefault (x:xs) = oneDefault xs


onlyErrors:: [TypeCheckRes] -> [TypeCheckRes]
onlyErrors [] = []
onlyErrors ((Errs str):xs) = (Errs str):(onlyErrors xs)
onlyErrors (x:xs) = onlyErrors xs


getTypeFromSwitchMatch:: SwitchMatch Posn -> Type -> Env -> TypeCheckRes
getTypeFromSwitchMatch (Match p expr bl) ty env =
  let rexprTy = getTypeFromRExpr expr env in 
  case rexprTy of
    Errs str -> Errs str
    _ ->
      if subType (getType rexprTy) ty
        then
          let foo = getTypeFromBlockDecl bl env in
          case foo of
            Errs strs -> Errs strs
            _ -> TypeInfo (getEnv foo) (SimpTyp T_Void) p dimVoid "" 0
        else Errs ["type " ++ (show $ getType rexprTy) ++ " not compatible with type " ++ (show ty) ++ " at " ++ (show p)]

getTypeFromSwitchMatch (Default p bl) _ env =
  let foo = getTypeFromBlockDecl bl env in
      case foo of
        Errs strs -> Errs strs
        _ -> TypeInfo (getEnv foo) (SimpTyp T_Void) p dimVoid "" 0


hasDuplicatesParams:: [Parameter Posn] -> Bool
hasDuplicatesParams list = length list /= length set
  where set = Set.fromList $ listOfId list


listOfId:: [Parameter Posn] -> [String]
listOfId [] = []
listOfId ((AbsGrammar.Param _ _ _ (Ident _ id) _):xs) = id:(listOfId xs)


hasReturn :: BlockDecl a -> Bool
hasReturn (Block _ stmts) = any isReturn stmts


isReturn :: Stmt a -> Bool
isReturn (Jmp _ (RetExp _ _)) = True
isReturn (Jmp _ (RetExpVoid _)) = True
isReturn s = False


-- Identifies the type of assgn
getAssgnType :: AbsGrammar.Assignment_op a -> Char
getAssgnType op = case op of
  AssgnMul _ -> 'n'
  AssgnAdd _ -> 'n'
  AssgnSub _ -> 'n'
  AssgnDiv _ -> 'n'
  AssgnPow _ -> 'n'
  AssgnAnd _ -> 'b'
  AssgnOr _ -> 'b'
  Assign _ -> 'o'


-- Takes a SelectionStmt and an environment returns the TypeCheckRes of that SelectionStmt or a list of Errors if present 
getTypeFromSelectionStmt :: SelectionStmt LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromSelectionStmt sel env = case sel of
  
  AbsGrammar.IfNoElse pos guard block -> case (getTypeFromRExpr guard env, getTypeFromBlockDecl block env) of
    (TypeInfo _ tg pg _ _ _, TypeInfo _ tb pb _ _ _) -> case (subType tg (SimpTyp T_Boolean), subType tb (SimpTyp T_Void)) of
      (True, True) -> TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0
      (False, False) -> Errs (("type " ++ (show tg) ++ " in " ++ (show pg) ++ " not compatible with expected type boolean"):("type " ++ (show tb) ++ " in " ++ (show pb) ++ " not compatible with expected type void"):[])
      (True, False) -> Errs (["type " ++ (show tb) ++ " in " ++ (show pb) ++ " not compatible with expected type void"])
      (False, True) -> Errs (["type " ++ (show tg) ++ " in " ++ (show pg) ++ " not compatible with expected type boolean"])
    (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
    (Errs strs, _) -> Errs strs
    (_, Errs strs) -> Errs strs
  
  AbsGrammar.IfElse pos g t e -> case (getTypeFromRExpr g env, getTypeFromBlockDecl t env, getTypeFromBlockDecl e env) of
    (TypeInfo _ tg pg _ _ _, TypeInfo _ tt pt _ _ _, TypeInfo _ te pe _ _ _) -> case (subType tg (SimpTyp T_Boolean), subType tt (SimpTyp T_Void), subType te (SimpTyp T_Void)) of
      (True, True, True) -> TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0
      (False, True, True) -> Errs ["type " ++ (show tg) ++ " in " ++ (show pg) ++ " not compatible with expected type boolean"]
      (True, False, _) -> Errs ["internal error (error code: 3)"]
      (True, _, False) -> Errs ["internal error (error code: 4)"]
      (False, False, _) -> Errs (("type " ++ (show tg) ++ " in " ++ (show pg) ++ " not compatible with expected type boolean"):("internal error (error code: 5)"):[])
      (False, _, False) -> Errs (("type " ++ (show tg) ++ " in " ++ (show pg) ++ " not compatible with expected type boolean"):("internal error (error code: 6)"):[])
    (Errs strs1, Errs strs2, Errs strs3) -> Errs (strs1 ++ strs2 ++ strs3)
    (Errs strs1, Errs strs2, _) -> Errs (strs1 ++ strs2)
    (Errs strs1, _, Errs strs3) -> Errs (strs1 ++ strs3)
    (_, Errs strs2, Errs strs3) -> Errs (strs2 ++ strs3)
    (_, _, Errs strs3) -> Errs strs3
    (_, Errs strs2, _) -> Errs strs2
    (Errs strs1, _, _) -> Errs strs1


-- Takes a BlockDecl and an environment returns the TypeCheckRes of that BlockDecl or a list of Errors if present 
getTypeFromBlockDecl :: BlockDecl LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromBlockDecl (Block pos stmts) env =
  let fenv = makeFunctionEnv stmts env [] in 
    checkStmts stmts (fst fenv) (snd fenv) pos env


-- Retuns the TypeCheckRes of the program, it calls getTypeFromBlockDecl because our program is inside the block
getTypeFromProgram :: Program LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromProgram (Prog _ block) env = getTypeFromBlockDecl block env


-- Takes a LExpr and an environment returns the TypeCheckRes of that LExpr or a list of Errors if present 
getTypeFromLExpr :: LExpr LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromLExpr lexpr env = case lexpr of
  
  BasLExpr _ _ bl -> getTypeFromBLExpr bl env

  IncDec pos _ _ operand -> case getTypeFromLExpr operand env of
    
    TypeInfo env2 (Type.Array _ _ _) _ _ _ _ -> Errs ["cannot use operation increment/decrement with array at " ++ (show pos)]
    
    TypeInfo env2 ty pos2 size id _ -> case (Data.Map.lookup id env, isIncDecCompliant ty) of
      (Just entries, True) -> case head entries of
        Var _ texpr True varDim _ _ _ -> TypeInfo env2 ty pos2 size id (getPosRow pos)
        Var _ texpr False varDim _ _ _ -> Errs ["cannot assign a value to a constant at " ++ show pos]
        Fun fPos _ _ _-> Errs ["cannot use function identifiers in L-expressions at " ++ (show fPos)]
      (Just _, False) -> Errs ["cannot use operation increment/decrement with type " ++ (show ty) ++ " at " ++ (show pos)]
      (Nothing, _) -> Errs ["variable " ++ id ++ " used in " ++ (show pos2) ++ " is undeclared"]
    Errs strs -> Errs strs

  Deref pos _ expr -> case getTypeFromRExpr expr env of
    TypeInfo _ (Point ty dim) _ _ id modl -> TypeInfo env ty pos dim id modl
    TypeInfo _ ty pos2 _ _ _ -> Errs ["type " ++ (show ty) ++ " not compatible with pointer type at " ++ (show pos2)]
    Errs strs -> Errs strs


-- given a Type, returns true if it is an int
isIncDecCompliant :: Type -> Bool
isIncDecCompliant (SimpTyp T_Int) = True
isIncDecCompliant _ = False


-- Takes a BLExpr and an environment returns the TypeCheckRes of that BLExpr or a list of Errors if present
getTypeFromBLExpr :: BLExpr LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromBLExpr blexpr env = case blexpr of
  Id pos _ _ id -> case Data.Map.lookup id env of
    Just entries -> case head entries of
      Var _ texpr _ s _ modl _ -> TypeInfo env texpr pos s id modl
      Fun fPos _ _ _ -> Errs ["cannot use function identifiers in L-expressions at " ++ (show fPos)]
    Nothing -> Errs ["variable " ++ id ++ " used in " ++ (show pos) ++ " is undeclared"]

  ArrayEl pos _ _ lexpr rexpr -> case (getTypeFromBLExpr lexpr env, getTypeFromRExpr rexpr env) of
    (TypeInfo _ (Type.Array te _ se) _ _ id modl, TypeInfo _ tr _ _ _ _) ->
      if subType tr (SimpTyp T_Int)
        then TypeInfo env te pos se id modl
        else Errs ["array index at " ++ (show pos) ++ " must be subtype of int"]
    (Errs strs1, Errs strs2) -> Errs (strs1 ++ strs2)
    (Errs strs, _) -> Errs strs
    (_, Errs strs) -> Errs strs
    (TypeInfo _ _ _ _ id _, _) -> Errs ["variable " ++ id ++ " used at " ++ (show pos) ++ " is not an array"]

  ParLExpr pos _ _ lexpr -> getTypeFromLExpr lexpr env


-- Takes a JumpStmt and an environment returns the TypeCheckRes of that JumpStmt or a list of Errors if present
getTypeFromJumpStmt :: JumpStmt LexGrammar.Posn -> Env -> TypeCheckRes
getTypeFromJumpStmt (Break pos) env = case Data.Map.lookup "while" env of
  Just entry -> TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0
  Nothing -> Errs ["unexpected break at " ++ (show pos)]

getTypeFromJumpStmt (Continue pos) env = case Data.Map.lookup "while" env of
  Just entry -> TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0
  Nothing -> Errs ["unexpected continue at " ++ (show pos)]

getTypeFromJumpStmt (RetExpVoid pos) env = case Data.Map.lookup "return" env of
  Just ((Var rPos rTy const dim _ _ _):xs) -> if (subType rTy (SimpTyp T_Void)) then (TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0) else (Errs ["type " ++ (show rTy) ++ " at " ++ (show pos) ++ " not compatible with type void declared for function"])
  Nothing -> Errs ["unexpected return at " ++ (show pos)]

getTypeFromJumpStmt (RetExp pos rExpr) env = case (Data.Map.lookup "return" env, getTypeFromRExpr rExpr env) of
  (Just ((Var rPos rTy const dim _ _ _):xs), Errs strs) -> Errs strs
  (Just ((Var rPos rTy const dim _ funRow _):xs), TypeInfo _ tr _ _ _ modr) -> case tr of
    Point _ _ -> if modr > funRow
                  then
                    Errs ["not sure if returned pointer points to a global variable at " ++ (show pos)]
                  else
                    if (subType tr rTy) then (TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0) else (Errs ["couldn't match expected type " ++ (show rTy) ++ " with actual type " ++ (show tr) ++ " at " ++ (show pos)])
    _ -> if (subType tr rTy) then (TypeInfo env (SimpTyp T_Void) pos dimVoid "" 0) else (Errs ["couldn't match expected type " ++ (show rTy) ++ " with actual type " ++ (show tr) ++ " at " ++ (show pos)])
  (Nothing, _) -> Errs ["unexpected return at " ++ (show pos)]


-- Takes a FCall, the function id, the list of parameters and an environment returns the TypeCheckRes of that FCall or a list of Errors if present
getTypeFromFCall:: LexGrammar.Posn -> String -> [RExpr Posn] -> Env -> TypeCheckRes
getTypeFromFCall pos id params env = case Data.Map.lookup id env of
  Just ((Fun fPos fParams fTy fDim):[]) -> case paramsCompliantCheck params fParams id pos env of
    Just str -> Errs str
    Nothing -> TypeInfo env fTy pos fDim "" 0
  Just entries -> case Prelude.filter (getCorrectFunctionToCall params id pos env) entries of
    [] -> Errs ["cannot call function " ++ id ++ " at " ++ (show pos) ++ ". Not valid parameters for all " ++ (show $ length entries) ++ " instances of function " ++ id]
    Fun _ _ fTy fDim:[] -> TypeInfo env fTy pos fDim "" 0
  Nothing -> Errs ["function " ++ id ++ " applied at " ++ (show pos) ++ " is not declared"]


getCorrectFunctionToCall :: [RExpr Posn] -> String -> LexGrammar.Posn -> Env -> EnvEntry -> Bool
getCorrectFunctionToCall aParams fName pos env (Fun fPos fParams fTy fDim) = case (paramsCompliantCheck aParams fParams fName pos env) of
  Just _ -> False
  Nothing -> True


-- Checks if the params in a function call are compliant with those in the funcion definition
paramsCompliantCheck :: [RExpr Posn] -> [Param] -> String -> LexGrammar.Posn -> Env -> Maybe [String]
paramsCompliantCheck aParams fParams fName pos env = 
  let errs = paramsCompliantCheckAux aParams fParams fName pos env 1 in 
    if Prelude.null errs then Nothing else Just errs


paramsCompliantCheckAux :: [RExpr Posn] -> [Param] -> String -> LexGrammar.Posn -> Env -> Integer -> [String]
paramsCompliantCheckAux [] [] _ _ _ _ = []
paramsCompliantCheckAux aParams fParams fName pos _ _ | (length aParams) /= (length fParams) =
  ["function call " ++ fName ++ " at " ++ (show pos) ++ " expected " ++ (show $ length fParams) ++ " arguments, but " ++ (show $ length aParams) ++ " has given"]
paramsCompliantCheckAux (act:xs) (for:ys) fName pos env _ | isJust $ isParametersPassedCorrect act for env = fromJust $ isParametersPassedCorrect act for env
paramsCompliantCheckAux aParams fParams fName pos env paramNum =
  case (getTypeFromRExpr (head aParams) env) of
    Errs strs -> strs
    _ -> let foo = if subType (getType $ getTypeFromRExpr (head aParams) env) (getParamType $ head fParams)
                      then []
                      else ["param number " ++ (show paramNum) ++ " of type " ++ (show . getType $ getTypeFromRExpr (head aParams) env) ++ " used in the function call " ++ fName ++ " at " ++ (show pos) ++ " not compatible with declared type " ++ (show $ getParamType $ head fParams)]
          in
            foo ++ (paramsCompliantCheckAux (tail aParams) (tail fParams) fName pos env (paramNum + 1))


isParametersPassedCorrect:: RExpr Posn -> Param -> Env -> Maybe [String]
isParametersPassedCorrect rexpr@(Lexpr _ _ _) (TypeChecking.Param ty _ Modality_ref _ _) env | isTypesEqual rexpr ty env = Nothing
isParametersPassedCorrect rexpr@(Lexpr _ _ _) (TypeChecking.Param ty _ Modality_valres _ _) env | isTypesEqual rexpr ty env = Nothing
isParametersPassedCorrect rexpr@(Lexpr _ _ _) (TypeChecking.Param ty _ Modality_ref _ _) _ = Just ["actual and formal parameters type must be equal in ref modality at " ++ (show $ rexprContent rexpr)]
isParametersPassedCorrect rexpr@(Lexpr _ _ _) (TypeChecking.Param ty _ Modality_valres _ _) _ = Just ["actual and formal parameters type must be equal in valres modality at " ++ (show $ rexprContent rexpr)]
isParametersPassedCorrect rexpr (TypeChecking.Param _ _ Modality_ref _ _) _ = Just ["cannot use simple type with ref modality at " ++ (show $ rexprContent rexpr)]
isParametersPassedCorrect rexpr (TypeChecking.Param _ _ Modality_valres _ _) _ = Just ["cannot use simple type with valres modality at " ++ (show $ rexprContent rexpr)]
isParametersPassedCorrect _ _ _ = Nothing


isTypesEqual:: RExpr Posn -> Type -> Env -> Bool
isTypesEqual rexpr ty env =
  case getTypeFromRExpr rexpr env of
    Errs _ -> True -- it should be "False", but it's True so the error is propagated (because it'll be raised in paramsCompliantCheckAux)
    _ -> ty == (getType $ getTypeFromRExpr rexpr env)


-- Takes a list of TypeCheckRes and retuns the most general TypeCheckRes for the TypeCheckRes in the list if it exists, returns an Error otherwise
getMostGeneralType :: [TypeCheckRes] -> TypeCheckRes
getMostGeneralType l = Prelude.foldl subTypeRes (TypeInfo empty Bottom (Pn 0 1 1) 0 "" 0) l


-- Takes 2 TypeCheckRes and returns the most general one if the types are compatible, Error otherwise
subTypeRes :: TypeCheckRes -> TypeCheckRes -> TypeCheckRes
subTypeRes (Errs l) (Errs l1) = Errs (l ++ l1)
subTypeRes (Errs l) _ = Errs l
subTypeRes _ (Errs l) = Errs l
subTypeRes (TypeInfo env1 type1 pos1 dim1 id1 _) (TypeInfo env2 type2 pos2 dim2 id2 _) =
  if compatibleType type1 type2
    then TypeInfo env2 type2 pos2 dim2 id2 0
    else if compatibleType type2 type1
      then TypeInfo env1 type1 pos1 dim1 id1 0
      else Errs ["array not homogeneous at " ++ show pos1]


-- Updates the env 
makeFunctionEnv :: [Stmt Posn] -> Env -> [String] -> (Env, [String])
makeFunctionEnv [] env errs = (env,errs)
makeFunctionEnv (x:xs) env errs = case x of

  AbsGrammar.FunDec pos (Ident pi id) [] retyp block -> case getTypeFromTypeSpec retyp empty Nothing of
    TypeInfo _ tre pre dre _ _ -> case Data.Map.lookup id env of
      Nothing -> makeFunctionEnv xs (insertWith firstEl id ((Fun pi [] tre dre):[]) env) errs
      Just entries ->
        if (or $ Prelude.map (conflictParameters []) (Prelude.map getEnvEntryPars entries))
          then makeFunctionEnv xs env (errs ++ [id ++ " is already defined"])
          else makeFunctionEnv xs (insertWith mergeEls id ((Fun pi [] tre dre):[]) env) errs
    Errs strs -> makeFunctionEnv xs env $ errs ++ strs
  
  AbsGrammar.FunDec pos (Ident pi id) pars retyp block -> case getTypeFromTypeSpec retyp empty Nothing of
      TypeInfo _ tre pre dre _ _ ->
        let checked = Prelude.map checkParameters pars in
        let envid = insertWith mergeEls id ((Fun pi (Data.Either.lefts checked) tre dre):[]) env in
          case (concat $ Data.Either.rights checked) of
            [] -> case Data.Map.lookup id env of 
              Nothing -> makeFunctionEnv xs envid errs
              Just entries ->
                if (or $ Prelude.map (conflictParameters $ Data.Either.lefts checked) (Prelude.map getEnvEntryPars entries))
                  then makeFunctionEnv xs env $ errs ++ [id ++ " is already defined"]
                  else makeFunctionEnv xs envid errs
            strs -> makeFunctionEnv xs env $ errs ++ strs
      Errs strs -> makeFunctionEnv xs env $ errs ++ strs
  
  _ -> makeFunctionEnv xs env errs


-- Given a list of Stmt(the block), an environment(for the block), a list of errors, a position and another environment (the external one) returns the TypeCheckRes of the block
checkStmts :: [Stmt Posn] -> Env -> [String] -> Posn -> Env -> TypeCheckRes

--if the block and errors lists are empty return a void type
checkStmts [] env [] pos ev = TypeInfo ev (SimpTyp T_Void) pos dimVoid "" 0

--if the block is emptybut the errors list isn't return the list of errors
checkStmts [] env (e:es) pos ev = Errs (e:es)

--if both are not empty check if the first stmt is an error list, if it is proceed with other stmts carrying the errors list over otherwise proceed with the other stmts carrying the  error list and create a new environment
checkStmts (x:xs) env (e:es) pos ev = case (getTypeFromStmt x env pos) of
  TypeInfo newEnv _ _ _ _ _-> checkStmts xs newEnv (e:es) pos ev
  Errs strs -> checkStmts xs env ((e:es) ++ strs) pos ev

--if the block is not empty and the error list is empty check if the first stmt is an error list, if it is proceed with other stmts creating the errors list otherwise proceed with the other stmts and create a new environment
checkStmts (x:xs) env [] pos ev = case (getTypeFromStmt x env pos) of
  TypeInfo newEnv _ _ _ _ _-> checkStmts xs newEnv [] pos ev
  Errs strs -> checkStmts xs env strs pos ev


-- Given a type returns the size of that type
getTypeSize :: Type -> Integer
getTypeSize t = case t of
  SimpTyp T_Boolean -> dimBool
  SimpTyp T_Char -> dimChar
  SimpTyp T_Float64 -> dimFloat64
  SimpTyp T_Int -> dimInt
  SimpTyp T_Void -> dimVoid
  SimpTyp T_String -> dimPointer
  Point _ _ -> dimPointer





{-
##################################################################################################
#################################### MAIN FUNCTIONS BELOW ########################################
##################################################################################################
-}








uselessTypeCheckRes = TypeInfo Data.Map.empty Bottom (Pn 0 1 1) 0 "" 0


-- Given a Program and an environment, substitutes the position of the nodes in the program with the relative type
computeProgram:: Program Posn -> Env -> Program TypeCheckRes
computeProgram input@(Prog _ block) env = Prog (getTypeFromProgram input env) (computeBlockDecl block env)

-- Given a RExpr node and an environment, substitutes the position of the node with the relative type
computeRExpr:: RExpr Posn -> Env -> RExpr TypeCheckRes
computeRExpr input@(BoolBinOp _ a b op1 op2) env = BoolBinOp (getTypeFromRExpr input env) a b (computeRExpr op1 env) (computeRExpr op2 env)
computeRExpr input@(Not _ a op) env = Not (getTypeFromRExpr input env) a (computeRExpr op env)
computeRExpr input@(Equality _ a b op1 op2) env = Equality (getTypeFromRExpr input env) a b (computeRExpr op1 env) (computeRExpr op2 env)
computeRExpr input@(Comparison _ a b op1 op2) env = Comparison (getTypeFromRExpr input env) a b (computeRExpr op1 env) (computeRExpr op2 env)
computeRExpr input@(BinOp _ a b op1 op2) env = BinOp (getTypeFromRExpr input env) a b (computeRExpr op1 env) (computeRExpr op2 env)
computeRExpr input@(Neg _ a op) env = Neg (getTypeFromRExpr input env) a (computeRExpr op env)
computeRExpr input@(Ref _ a op) env = Ref (getTypeFromRExpr input env) a (computeLExpr op env)
computeRExpr input@(FCall _ a (Ident pos id) op) env = FCall (getTypeFromRExpr input env) a (Ident (uselessTypeCheckRes {getPos = pos}) id) (Prelude.map (\x -> computeRExpr x env) op)
computeRExpr (Int po a (MyInteger _ b)) _ = Int (TypeInfo Data.Map.empty (SimpTyp T_Int) po dimInt "" 0) a (MyInteger uselessTypeCheckRes b)
computeRExpr (Char po a (MyChar _ b)) _ = Char (TypeInfo Data.Map.empty (SimpTyp T_Char) po dimChar "" 0) a (MyChar uselessTypeCheckRes b)
computeRExpr (String po a (MyString _ b)) _ = String (TypeInfo Data.Map.empty (SimpTyp T_String) po dimPointer "" 0) a (MyString uselessTypeCheckRes b)
computeRExpr (Float po a (MyDouble _ b)) _ = Float (TypeInfo Data.Map.empty (SimpTyp T_Float64) po dimFloat64 "" 0) a (MyDouble uselessTypeCheckRes b)
computeRExpr (Bool po a b) _ = Bool (TypeInfo Data.Map.empty (SimpTyp T_Boolean) po dimBool "" 0) a (b {booleanContent = uselessTypeCheckRes})
computeRExpr input@(Lexpr _ a expr) env = Lexpr (getTypeFromRExpr input env) a (computeLExpr expr env)
computeRExpr input@(IfRe _ a guard the els) env = IfRe (getTypeFromRExpr input env) a (computeRExpr guard env) (computeRExpr the env) (computeRExpr els env)

-- Given a LExpr node and an environment, substitutes the position of the node with the relative type
computeLExpr:: LExpr Posn -> Env -> LExpr TypeCheckRes
computeLExpr input@(Deref _ a op) env = Deref (getTypeFromLExpr input env) a (computeRExpr op env)
computeLExpr input@(IncDec _ a b op) env = IncDec (getTypeFromLExpr input env) a b (computeLExpr op env)
computeLExpr input@(BasLExpr _ a op) env = BasLExpr (getTypeFromLExpr input env) a (computeBLExpr op env)

-- Given a BLExpr node and an environment, substitutes the position of the node with the relative type
computeBLExpr:: BLExpr Posn -> Env -> BLExpr TypeCheckRes
computeBLExpr input@(ArrayEl _ a b op1 op2) env = ArrayEl (getTypeFromBLExpr input env) a b (computeBLExpr op1 env) (computeRExpr op2 env)
computeBLExpr input@(Id _ a b c) env = Id (getTypeFromBLExpr input env) a b c
computeBLExpr input@(ParLExpr _ a b l) env = ParLExpr (getTypeFromBLExpr input env) a b (computeLExpr l env)

-- Given a Stmt node, an environment and a Posn, substitutes the position of the node with the relative type
computeStmt :: Stmt Posn -> Env -> Posn -> Stmt TypeCheckRes
computeStmt input@(Comp _ block) env pos = Comp (getTypeFromStmt input env pos) (computeBlockDecl block env)
computeStmt input@(ProcCall _ (Ident pi id) pars) env pos = ProcCall (getTypeFromStmt input env pos) (Ident (uselessTypeCheckRes {getPos = pi}) id) (Prelude.map (\x -> computeRExpr x env) pars)
computeStmt input@(Jmp _ jump) env pos = Jmp (getTypeFromStmt input env pos) (computeJumpStmt jump env)
computeStmt input@(While _ guard block) env pos = While (getTypeFromStmt input env pos) (computeRExpr guard env) (computeBlockDecl block $ insertWith firstEl "while" [Var (Pn 0 1 1) Bottom True 0 Modality1 0 False] env)
computeStmt input@(DoWhile _ guard block) env pos = DoWhile (getTypeFromStmt input env pos) (computeRExpr guard env) (computeBlockDecl block $ insertWith firstEl "while" [Var (Pn 0 1 1) Bottom True 0 Modality1 0 False] env)
computeStmt input@(For _ id st fi bl) env pos = For (getTypeFromStmt input env pos) (id {identContent = uselessTypeCheckRes}) (computeRExpr st env) (computeRExpr fi env) (computeBlockDecl bl $ insertWith mergeEls (identString id) [Var pos (SimpTyp T_Int) False dimInt Modality1 0 False] $ insertWith mergeEls "while" [Var (Pn 0 1 1) Bottom False 0 Modality1 0 False] env)
computeStmt input@(Switch p expr block) env pos = Switch (getTypeFromStmt input env pos) (computeRExpr expr env) (computeSwitchBlock block (getType $ getTypeFromRExpr expr env) env)
computeStmt input@(Sel _ sel) env pos = Sel (getTypeFromStmt input env pos) (computeSelectionStmt sel env)
computeStmt input@(Assgn _ l op r) env pos = Assgn (getTypeFromStmt input env pos) (computeLExpr l env) (op {assignmentOpContent = uselessTypeCheckRes}) (computeRExpr r env)
computeStmt input@(LExprStmt _ l) env pos = LExprStmt (getTypeFromStmt input env pos) (computeLExpr l env)
computeStmt input@(VarDec _ decl) env pos = VarDec (getTypeFromStmt input env pos) (computeVarDeclInit decl env pos)
computeStmt input@(FunDec _ (Ident pi id) pars retyp block) env pos = case (getTypeFromTypeSpec retyp empty Nothing) of
      TypeInfo _ tre pre dre _ _ -> let checked = (Prelude.map checkParameters pars) in
        FunDec (getTypeFromStmt input env pos) (Ident (uselessTypeCheckRes {getPos = pi}) id) [] (computeTypeSpec retyp env Nothing) (computeBlockDecl block (insertWith firstEl "return" ((Var pre tre False dre Modality1 (getPosRow pos) False):[]) (addParameters pars env)))

computeSwitchBlock:: SwitchBlock Posn -> Type -> Env -> SwitchBlock TypeCheckRes
computeSwitchBlock sBlock@(BlockSwitch pos match) ty env = BlockSwitch (getTypeFromSwitchBlock sBlock ty env) (Prelude.map (\x -> computeSwitchMatch x ty env) match)

computeSwitchMatch:: SwitchMatch Posn -> Type -> Env -> SwitchMatch TypeCheckRes
computeSwitchMatch input@(Match po rexpr bl) ty env = Match (getTypeFromSwitchMatch input ty env) (computeRExpr rexpr env) (computeBlockDecl bl env)
computeSwitchMatch input@(Default po bl) ty env = Default (getTypeFromSwitchMatch input ty env) (computeBlockDecl bl env)

-- Given a BlockDecl node and an environment, substitutes the position of the node with the relative type
computeBlockDecl :: BlockDecl Posn -> Env -> BlockDecl TypeCheckRes
computeBlockDecl input@(Block pos stmts) env = let fenv = (fst (makeFunctionEnv stmts env [])) in
  Block (getTypeFromBlockDecl input env) (computeStmtList stmts fenv pos)

-- Given a list of Stmt node, an environment and a Posn, substitutes the position of the node with the relative type
computeStmtList:: [Stmt Posn] -> Env -> Posn -> [Stmt TypeCheckRes]
computeStmtList [] _ _ = []
computeStmtList (x:xs) env pos = case (getTypeFromStmt x env pos) of
  TypeInfo newEnv t p d i _ -> (computeStmt x env pos):(computeStmtList xs newEnv pos)
  Errs strs -> (computeStmt x env pos):(computeStmtList xs env pos)

-- Given a TypeSpec node and an environment, substitutes the position of the node with the relative type
computeTypeSpec:: TypeSpec Posn -> Env -> Maybe Type ->TypeSpec TypeCheckRes
computeTypeSpec input@(BasTyp _ ro) env arr = BasTyp (getTypeFromTypeSpec input env arr) (ro { basicTypeContent = uselessTypeCheckRes})
computeTypeSpec input@(ArrDef _ ty s) env arr = case arr of
  Just (Type.Array te len se) -> ArrDef  (getTypeFromTypeSpec input env arr) (computeTypeSpec ty env (Just te)) (s {myIntegerContent = uselessTypeCheckRes})
  _ -> ArrDef  (getTypeFromTypeSpec input env arr) (computeTypeSpec ty env Nothing) (s {myIntegerContent = uselessTypeCheckRes})

computeTypeSpec input@(ArrUnDef _ ty) env arr = case arr of
  Just (Type.Array te len se) -> ArrUnDef (getTypeFromTypeSpec input env arr) (computeTypeSpec ty env (Just te))
  _ -> ArrUnDef (getTypeFromTypeSpec input env arr) (computeTypeSpec ty env Nothing)

computeTypeSpec input@(Pointer _ ro) env arr = case arr of
  Just (Point tp _) -> Pointer (getTypeFromTypeSpec input env arr) (computeTypeSpec ro env (Just tp))
  _ -> Pointer (getTypeFromTypeSpec input env arr) (computeTypeSpec ro env Nothing)


-- Given a VarDeclInit node, an environment and a Posn, substitutes the position of the node with the relative type
computeVarDeclInit:: VarDeclInit Posn -> Env -> Posn -> VarDeclInit TypeCheckRes
computeVarDeclInit input@(VarDeclIn _ a (Ident pi id) ty crexp check) env pos = VarDeclIn (getTypeFromVarDeclInit input env pos) a (Ident (uselessTypeCheckRes {getPos = pi}) id) (computeTypeSpec ty env (getMaybeTypeFromComplexRExpr crexp env)) (computeComplexRExpr crexp env) check
computeVarDeclInit input@(ConDeclIn _ a (Ident pi id) ty crexp check) env pos = ConDeclIn (getTypeFromVarDeclInit input env pos) a (Ident (uselessTypeCheckRes {getPos = pi}) id) (computeTypeSpec ty env (getMaybeTypeFromComplexRExpr crexp env)) (computeComplexRExpr crexp env) check

-- Given a TypeSpec node and an environment, substitutes the position of the node with the relative type
computeComplexRExpr:: ComplexRExpr Posn -> Env -> ComplexRExpr TypeCheckRes
computeComplexRExpr input@(Simple _ a rexp) env = Simple (getTypeFromComplexRExpr input env) a (computeRExpr rexp env)
computeComplexRExpr input@(AbsGrammar.Array _ a lis) env = AbsGrammar.Array (getTypeFromComplexRExpr input env) a (Prelude.map(\x-> computeComplexRExpr x env) lis)

-- Given a JumpStmt node and an environment, substitutes the position of the node with the relative type
computeJumpStmt:: JumpStmt Posn  -> Env -> JumpStmt TypeCheckRes
computeJumpStmt input@(Break _) env = Break (getTypeFromJumpStmt input env)
computeJumpStmt input@(Continue _) env = Continue (getTypeFromJumpStmt input env)
computeJumpStmt input@(RetExpVoid _) env = RetExpVoid (getTypeFromJumpStmt input env)
computeJumpStmt input@(RetExp _ re) env = RetExp (getTypeFromJumpStmt input env) (computeRExpr re env)

-- Given a SelectionStmt node and an environment, substitutes the position of the node with the relative type
computeSelectionStmt:: SelectionStmt Posn -> Env -> SelectionStmt TypeCheckRes
computeSelectionStmt input@(IfNoElse _ guard the) env = IfNoElse (getTypeFromSelectionStmt input env) (computeRExpr guard env) (computeBlockDecl the env)
computeSelectionStmt input@(IfElse _ guard the els) env = IfElse (getTypeFromSelectionStmt input env) (computeRExpr guard env) (computeBlockDecl the env) (computeBlockDecl els env)
