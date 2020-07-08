module TACGen where

import LexGrammar
import ParGrammar
import SkelGrammar
import AbsGrammar
import TypeChecking
import Type

import Data.Map
import Data.Char
import Data.Maybe
import Data.List



-- TAC for code, TAC for functions, TAC for Strings
data TACS = TACS {text :: [TAC], functions :: [TAC], dataMem :: [TAC]}


data TAC
  = TacAssignBinOp {getAddrResult:: Addr, getBinOp:: TBinOp, getFstAddr:: Addr, getSndAddr:: Addr, getTypeAssign:: String}
  | TacAssignRelOp {getAddrResult:: Addr, getBoolBinOp:: TRelOp, getFstAddr:: Addr, getSndAddr:: Addr, getTypeAssign:: String}
  | TacAssignUnOp {getAddrResult:: Addr, getUnOp:: TUnOp, getFstAddr:: Addr, getTypeAssign:: String}
  | TacCopy Addr Addr String
  | TacJump Lab
  | TacConJump Lab Bool Addr
  | TacRelConJump Lab Bool TRelOp Addr Addr
  | TacParam Addr String
  | TacProcCall Addr Int
  | TacFunCall Addr Int Addr String
  | TacArrayRead Addr Addr Addr String
  | TacArrayWrite Addr Addr Addr String
  | TacAssignRef Addr Addr
  | TacAssignDeref Addr Addr String
  | TacWritePointee Addr Addr String
  | TacLabel Lab
  | TacRetVoid
  | TacRet Addr String
  | TacString Addr [Addr]
  | TacComment String
  | TacError String
  | TacExit

instance Eq TAC where
  (==) (TacString s1 _) (TacString s2 _) = (addrString s1) == (addrString s2)

instance Ord TAC where
  (<=) (TacString s1 _) (TacString s2 _) = (addrString s1) <= (addrString s2)
 

data TBinOp = IntAdd | FloatAdd | LongAdd | IntSub | FloatSub | LongSub | IntMul | FloatMul | IntDiv | FloatDiv | IntPow | FloatPow | Mod

data TRelOp = Or | And | EqInt | EqLong | EqChar | EqFloat | EqBoolean | NeqInt | NeqLong | NeqChar | NeqFloat | NeqBoolean | LtInt | LtLong | LtChar | LtFloat | LtBoolean | LtEInt | LtELong | LtEChar | LtEFloat | LtEBoolean | GtInt | GtLong | GtChar | GtFloat | GtBoolean | GtEInt | GtELong | GtEChar | GtEFloat | GtEBoolean

  
data TUnOp = Neg | IntToFloat | PreInc | PostInc | PreDecr | PostDecr
  deriving (Show)


instance Show TBinOp where
  show tu = case tu of
    IntAdd -> "add_int"
    IntSub -> "sub_int"
    IntMul -> "mul_int"
    IntDiv  -> "div_int"
    IntPow -> "pow_int"
    FloatAdd -> "add_float"
    FloatSub -> "sub_float"
    FloatMul -> "mul_float"
    FloatDiv -> "div_float"
    FloatPow -> "pow_float"
    LongAdd -> "add_long"
    LongSub -> "sub_long"
    TACGen.Mod -> "mod_int"

    
instance Show TRelOp where
  show t = case t of
    TACGen.Or -> "or"
    TACGen.And -> "and"
    TACGen.EqInt -> "eq_int"
    TACGen.EqLong -> "eq_long"
    TACGen.EqChar -> "eq_char"
    TACGen.EqFloat -> "eq_float"
    TACGen.EqBoolean -> "eq_boolean"
    TACGen.NeqInt -> "neq_int"
    TACGen.NeqLong -> "neq_long"
    TACGen.NeqChar -> "neq_char"
    TACGen.NeqFloat -> "neq_float"
    TACGen.NeqBoolean -> "neq_boolean"
    TACGen.LtInt -> "lt_int"
    TACGen.LtLong -> "lt_long"
    TACGen.LtChar -> "lt_char"
    TACGen.LtFloat -> "lt_float"
    TACGen.LtBoolean -> "lt_boolean"
    TACGen.LtEInt -> "lte_int"
    TACGen.LtELong -> "lte_long"
    TACGen.LtEChar -> "lte_char"
    TACGen.LtEFloat -> "lte_float"
    TACGen.LtEBoolean -> "lte_boolean"
    TACGen.GtInt -> "gt_int"
    TACGen.GtLong -> "gt_long"
    TACGen.GtChar -> "gt_char"
    TACGen.GtFloat -> "gt_float"
    TACGen.GtBoolean -> "gt_boolean"
    TACGen.GtEInt -> "gte_int"
    TACGen.GtELong -> "gte_long"
    TACGen.GtEChar -> "gte_char"
    TACGen.GtEFloat -> "gte_float"
    TACGen.GtEBoolean -> "gte_boolean"
    

instance Show TAC where
  show tac = case tac of

    TacAssignBinOp d op l r t -> "\t" ++ (show d) ++ " =" ++ t ++ " " ++ (show l) ++ " " ++ (show op) ++ " " ++ (show r)
    TacAssignRelOp d op l r t -> "\t" ++ (show d) ++ " =" ++ t ++ " " ++ (show l) ++ " " ++ (show op) ++ " " ++ (show r)
    TacAssignUnOp d op s t -> "\t" ++ (show d) ++ " =" ++ t ++ " " ++ (show op) ++ " " ++ (show s)
    TacCopy d s t -> "\t" ++ (show d) ++ " =" ++ t ++ " " ++ (show s)
    TacJump j -> "\tgoto " ++ j
    TacConJump j b a -> case b of
      True -> "\tif " ++ (show a) ++ " goto " ++ j
      False -> "\tifFalse " ++ (show a) ++ " goto " ++ j
    TacRelConJump j b op l r -> case b of
      True -> "\tif " ++ (show l) ++ " " ++ (show op) ++ " " ++ (show r) ++ " goto " ++ j
      False -> "\tifFalse " ++ (show l) ++ " " ++ (show op) ++ " " ++ (show r) ++ " goto " ++ j
    TacParam p t -> "\tparam " ++ (show p) ++ "_" ++ t
    TacProcCall id n -> "\tcall " ++ (show id) ++ ":" ++ (show n)
    TacFunCall id n d t -> "\t" ++ (show d) ++ " =" ++ t ++ " call " ++ (show id) ++ ":" ++ (show n)
    TacArrayRead d a o t -> "\t" ++ (show d) ++ " =" ++ t ++ " " ++ (show a) ++ "[" ++ (show o) ++ "]"
    TacArrayWrite a o s t -> "\t" ++ (show a) ++ "[" ++ (show o) ++ "]" ++ " =" ++ t ++ " " ++ (show s)
    TacAssignRef d s -> "\t" ++ (show d) ++ " =addr " ++ "&" ++ (show s)
    TacAssignDeref d s t -> "\t" ++ (show d) ++ " =" ++ t ++ " *" ++ (show s)
    TacWritePointee d s t -> "\t*" ++ (show d) ++ " =" ++ t ++ " " ++ (show s)
    TacLabel l -> l ++ ":"
    TacRetVoid -> "\treturn"
    TacRet s t -> "\treturn_" ++ t ++ " " ++ (show s)
    TacString val ps ->
      let strs = (Prelude.map show ps) in
        (Prelude.foldl (\x y -> x ++ ":\n" ++ y) (head strs) (tail strs)) ++ ":\n\t" ++ (show val)
    TacComment str -> "# " ++ str
    TacError str -> "error " ++ (show str)
    TacExit -> "\texit"

printTAC ts = sequence $ Prelude.map (putStrLn . show) ts


-- given the TAC of the program, returns the Tac in the reverse order
getTAC :: Program TACS -> [TAC]
getTAC (Prog (TACS t f d) _) = [TacComment "static data"] ++ (mergeStrs (sort d)) ++ [TacComment "text"] ++ ((reverse t) ++ [TacComment "functions"] ++ (reverse f))


mergeStrs :: [TAC] -> [TAC]
mergeStrs [] = []
mergeStrs (str:[]) = (str:[])
mergeStrs (s1:s2:strs) = case (s1, s2) of
  (TacString c1 ps1, TacString c2 ps2) -> if (c1 == c2) then mergeStrs ((TacString c1 (ps1 ++ ps2)):strs) else s1:(mergeStrs (s2:strs))



-- Given the program returns the TAC for the program
getTACFromProgram :: Program TypeCheckRes -> Program TACS
getTACFromProgram (Prog _ block) = 
  let end = newLab 0 in
  let tacblock = fst $ getTACFromBlockDecl block 1 end "" in
  let tacs = (blockDeclContent tacblock) in
    Prog (tacs {text = ((TacExit):(TacLabel end):(text tacs))}) tacblock


-- Given a BlockDecl, a counter, the next label and a label for the instruction after a while cicle, returns a couple (BlockDecl TACS, updated counter)
getTACFromBlockDecl :: BlockDecl TypeCheckRes -> Integer -> Lab -> Lab -> (BlockDecl TACS, Integer)
getTACFromBlockDecl (Block _ stmts) c next nextWhile = 
  let num = length stmts in
    case num of
      0 -> (Block (TACS [] [] []) [], c)
      1 ->
        let tacs = fst $ getTACFromStmt (head stmts) c next nextWhile in
        let d = snd $ getTACFromStmt (head stmts) c next nextWhile in
        
          (Block (stmtContent tacs) [tacs], d)
      _ ->
        let lnum = c + (toInteger $ num - 2) in
        let labs = Prelude.map newLab [c..lnum] in
        let tac2 = getTACFromStmtList (c + (toInteger num)) (reverse $ next:labs) stmts nextWhile in
        let tacstmts = fst tac2 in
        let d = snd tac2 in
          
          (Block (makeBlockCode labs $ reverse $ Prelude.map stmtContent tacstmts) tacstmts, d)


-- Given a Stmt, a counter, the next label and a label for the instruction after a while cicle, returns a couple (Stmt TACS, updated counter)
getTACFromStmt :: Stmt TypeCheckRes -> Integer -> Lab -> Lab -> (Stmt TACS, Integer)
getTACFromStmt stmt c next nextWhile = case stmt of
  
  Comp _ decl ->
    let tacdecl = fst $ getTACFromBlockDecl decl c next nextWhile in
    let d = snd $ getTACFromBlockDecl decl c next nextWhile in
      
      (Comp (blockDeclContent tacdecl) tacdecl, d)
  
  VarDec _ decl ->
    let tacdecl = fst $ getTACFromVarDeclInit decl c in
    let d = snd $ getTACFromVarDeclInit decl c in
    
      (VarDec (varDeclInitContent tacdecl) tacdecl, d)
  
  Sel _  selstmt ->
    let tacstmt = fst $ getTACFromSelectionStmt selstmt c next nextWhile in
    let d = snd $ getTACFromSelectionStmt selstmt c next nextWhile in
    
      (Sel (selectionStmtContent tacstmt) tacstmt, d)
  
  AbsGrammar.While _ guard block ->
    let begin = newLab c in
    let startBody = newLab $ c + 1 in
    let tacr = (fst $ getTACFromRExpr guard (c + 2) startBody fall) in
    let d = (snd $ getTACFromRExpr guard (c + 2) startBody fall) in
    let tacb = (fst $ getTACFromBlockDecl block d begin next) in
    let e = (snd $ getTACFromBlockDecl block d begin next) in
    let rtacs = (rexprContent tacr) in
    let btacs = (blockDeclContent tacb) in
    let t = ((text rtacs) ++ [TacLabel begin] ++ (text btacs) ++ [TacLabel startBody, TacJump begin]) in
    
      (AbsGrammar.While (TACS t (functions btacs) ((dataMem rtacs) ++ (dataMem btacs))) tacr tacb, e)

  AbsGrammar.DoWhile tres guard block ->
    let while = (AbsGrammar.While tres guard block) in
    let tacw = (fst $ getTACFromStmt while c next nextWhile) in
    let d = (snd $ getTACFromStmt while c next nextWhile) in
    let wtacs = (stmtContent tacw) in
    let cont = (wtacs {text = (init (text wtacs))}) in

      (tacw {stmtContent = cont}, d)

  AbsGrammar.For ty loopVar st fi block ->
    let begin = newLab c in
    let stTac = getValFromRExpr st $ c + 1 in
    let fiTac = getValFromRExpr fi (snd stTac) in
    let fiTemp = newTemp $ snd fiTac in
    let fiCopy = TacCopy fiTemp (rexprAddr $ fst fiTac) "int" in
    let blockTac = getTACFromBlockDecl block (snd fiTac + 1) begin next in
    let iden = getIdentifier (varPos . head . fromJust $ Data.Map.lookup (identString loopVar) (getEnv ty)) (identString loopVar) in
    let finishBody = newLab $ snd blockTac in
    let incr = TacAssignBinOp iden IntAdd iden (CInt 1) "int" in
    let tacCopy = TacCopy iden (rexprAddr $ fst stTac) (getTACType (SimpTyp T_Int)) in
    let t = [TacLabel finishBody] ++ [TacJump begin] ++ [incr] ++ (text . blockDeclContent $ fst blockTac) ++[TacRelConJump finishBody True GtInt (iden) fiTemp] ++ [TacLabel begin] ++ [tacCopy] ++ [fiCopy] ++ (text . rexprContent $ fst fiTac) ++ (text . rexprContent $ fst stTac) in

      (AbsGrammar.For (TACS t (functions . blockDeclContent $ fst blockTac) ((dataMem . rexprContent $ fst stTac) ++ (dataMem . rexprContent $ fst fiTac))) (Ident (TACS [] [] []) "") (fst stTac) (fst fiTac) (fst blockTac), (+) 1 $ snd blockTac)

  AbsGrammar.Assgn ty lexpr op rexpr ->
    let castDest = newTemp c in
    let opDest = newTemp (c+1) in
    let typl = (getType $ lexprContent lexpr) in
    let typr = (getType $ rexprContent rexpr) in
    let tacr = (fst $ getValFromRExpr rexpr (c+2)) in
    let d = (snd $ getValFromRExpr rexpr (c+2)) in
    let rtacs = (rexprContent tacr) in
      case (getAssgnType op) of
        'o' ->
          let source = if (typl == typr) then rexprAddr tacr else castDest in
          let tacl = (fst $ getAssignTACFromLExpr lexpr d source) in
          let e = (snd $ getAssignTACFromLExpr lexpr d source) in
            if typl == typr
              then
                (AbsGrammar.Assgn (TACS ((text (lexprContent tacl)) ++ (text rtacs)) [] ((dataMem (lexprContent tacl)) ++ (dataMem rtacs))) tacl (op {assignmentOpContent = (TACS [] [] [])}) tacr, e)
              else
                (AbsGrammar.Assgn (TACS ((text (lexprContent tacl)) ++ ((cast source (rexprAddr tacr) typl typr):(text rtacs))) [] ((dataMem (lexprContent tacl)) ++ (dataMem rtacs))) tacl (op {assignmentOpContent = (TACS [] [] [])}) tacr,  e)
        _ ->
          let source = opDest in
          let tacvall = (fst $ getTACFromLExpr lexpr d) in
          let e = (snd $ getTACFromLExpr lexpr d) in
          let tacl = (fst $ getAssignTACFromLExpr lexpr e source) in
          let f = (snd $ getAssignTACFromLExpr lexpr e source) in
            if typl == typr
              then
                (AbsGrammar.Assgn (TACS ((text (lexprContent tacl)) ++ ((makeOp opDest (lexprAddr tacvall) op (rexprAddr tacr) typl):((text (lexprContent tacvall))++(text rtacs)))) [] ((dataMem (lexprContent tacl)) ++ (dataMem rtacs))) tacl (op {assignmentOpContent = (TACS [] [] [])}) tacr,  f)
              else
                (AbsGrammar.Assgn (TACS ((text (lexprContent tacl)) ++ ((makeOp opDest (lexprAddr tacvall) op castDest typl):(cast castDest (rexprAddr tacr) typl typr):((text (lexprContent tacvall))++(text rtacs)))) [] ((dataMem (lexprContent tacl)) ++ (dataMem rtacs))) tacl (op {assignmentOpContent = (TACS [] [] [])}) tacr,  f)

  AbsGrammar.Jmp ty jmp ->
    let jump = getTACFromJumpStmt jmp c next nextWhile in
      
      (AbsGrammar.Jmp (jumpStmtContent(fst jump)) (fst jump), snd jump)  

  AbsGrammar.LExprStmt ty lexpr ->
    let tacl = getTACFromLExpr lexpr c in 
      (AbsGrammar.LExprStmt (lexprContent $ fst tacl) (fst tacl) , snd tacl)
 
  AbsGrammar.ProcCall ty id params -> getTACFromProcCall ty (CString "") (identString id) params c

  AbsGrammar.FunDec tres (Ident _ id) _ tret body ->
    let line = (getPosRow (getPos tres)) in
    let func = head $ Prelude.filter (funAt line) (findWithDefault [] id $ getEnv tres) in
    let funid = (getIdentifier (funPos func) id) in
    let partac = (getTACFromParamList (funParams func)) in
    let post = (snd partac) ++ [TacComment "postamble"] in
    let btacs = (blockDeclContent (fst (getTACFromBlockDecl body c next nextWhile))) in
    let d = (snd (getTACFromBlockDecl body c next nextWhile)) in
    let end = if ((funType func) == (SimpTyp T_Void)) then [TacRetVoid] else [TacError ("control of function " ++ (show funid) ++ " should not reach this point.")] in
    let tac = if partac == ([],[])
                then
                  (TACS [] ((functions btacs) ++ end ++ (text btacs) ++ [TacComment ("code of " ++ (show funid)), TacLabel (show funid)]) (dataMem btacs))
                else
                  (TACS [] ((functions btacs) ++ end ++ post ++ (applyPost post (text btacs)) ++ [TacComment ("code of " ++ (show funid))] ++ (fst partac) ++ [TacComment "init", TacLabel (show funid)]) (dataMem btacs)) in
      (AbsGrammar.FunDec tac (Ident (TACS [] [] []) id) [] (getTACFromTypeSpec tret) (fst (getTACFromBlockDecl body c next nextWhile)), d)

  AbsGrammar.Goto _ lab -> (AbsGrammar.Goto (TACS [TacJump lab] [] []) lab, c)

  AbsGrammar.Switch _ op mlist ->
    let newT = (newTemp c) in
    let opType = (getType (rexprContent op)) in
    let tacr = (fst $ getValFromRExpr op (c+1)) in
    let d = (snd $ getValFromRExpr op (c+1)) in
    let begtacs = (mergeTACS (TACS [TacCopy newT (rexprAddr tacr) (getTACType opType)] [] []) (rexprContent tacr)) in
    let tacls = (fst $ getTACFromSwitchBlock mlist d next nextWhile newT opType next) in
    let e = (snd $ getTACFromSwitchBlock mlist d next nextWhile newT opType next) in
      (AbsGrammar.Switch (mergeTACS (switchBlockContent tacls) begtacs) tacr tacls, e)

getTACFromSwitchBlock :: SwitchBlock TypeCheckRes -> Integer -> Lab -> Lab -> Addr -> Type -> Lab -> (SwitchBlock TACS, Integer)
getTACFromSwitchBlock matches c next nextWhile opAdd opType endL =
  let stmts = Prelude.map (makeIfBlock) (makeEqBlocks (switchBlockMatches matches) opAdd opType endL) in
  let switchBlock = Block (stmtContent (head stmts)) stmts in
  let tacs = (fst $ getTACFromBlockDecl switchBlock c next nextWhile) in
  let d = (snd $ getTACFromBlockDecl switchBlock c next nextWhile) in
    (AbsGrammar.BlockSwitch (blockDeclContent tacs) [], d)

makeEqBlocks :: [SwitchMatch TypeCheckRes] -> Addr -> Type -> Lab -> [(RExpr TypeCheckRes, BlockDecl TypeCheckRes)]
makeEqBlocks matches opAdd opType endL =
  let end = [preDefault (last matches)] in
    (Prelude.map (makeEq opAdd opType endL) (init matches)) ++ end

preDefault :: SwitchMatch TypeCheckRes -> (RExpr TypeCheckRes, BlockDecl TypeCheckRes)
preDefault (Default _ block) = (AbsGrammar.Bool ((blockDeclContent block) {getType = (SimpTyp T_Boolean), getSize = dimBool}) (CString "") (AbsGrammar.Boolean_true uselessTypeCheckRes), block)

makeEq :: Addr -> Type -> Lab -> SwitchMatch TypeCheckRes -> (RExpr TypeCheckRes, BlockDecl TypeCheckRes)
makeEq opAdd opType endL (Match _ rexpr block) =
  let opRexpr = AbsGrammar.PreCalc ((blockDeclContent block) {getType = opType, getSize = (getTypeSize opType)}) opAdd in
  let eq = AbsGrammar.Equality ((blockDeclContent block) {getType = (SimpTyp T_Boolean), getSize = dimBool}) (CString "") AbsGrammar.Eq opRexpr rexpr in
    (eq, (addJumpToBlock block endL))

makeIfBlock :: (RExpr TypeCheckRes, BlockDecl TypeCheckRes) -> Stmt TypeCheckRes
makeIfBlock (guard, block) =
  let tres = (blockDeclContent block) in
  let selIf = AbsGrammar.IfNoElse tres guard block in
    AbsGrammar.Sel tres selIf

funAt :: Int -> (EnvEntry -> Bool)
funAt num = \x -> case x of
  Fun (Pn _ row _) _ _ _ -> row == num
  _ -> False

addJumpToBlock :: BlockDecl TypeCheckRes -> Lab -> BlockDecl TypeCheckRes
addJumpToBlock (Block tres stmts) dest =
  let goto = [AbsGrammar.Goto uselessTypeCheckRes dest] in
    Block tres (stmts ++ goto)

getTACFromParamList :: [Param] -> ([TAC],[TAC])
getTACFromParamList [] = ([],[])


getTACFromParamList (p:ps) = case p of
  TypeChecking.Param t pos Modality_valres str _->
    let paramId = (getIdentifier pos str) in
    let local = (CAddr ((show paramId) ++ "$valres")) in
      ((TacAssignDeref local paramId (getTACType t)):(fst $ getTACFromParamList ps),(TacWritePointee paramId local (getTACType t)):(snd $getTACFromParamList ps))
  _ -> getTACFromParamList ps


applyPost :: [TAC] -> [TAC] -> [TAC]
applyPost _ [] = []


applyPost post ((TacRet a b):ts) = ((TacRet a b):post) ++ (applyPost post ts)
applyPost post (TacRetVoid:ts) = ((TacRetVoid):post) ++ (applyPost post ts)
applyPost post (t:ts) = (t:(applyPost post ts))


-- Creates the right kind of operation
makeOp :: Addr -> Addr -> Assignment_op a -> Addr -> Type -> TAC
makeOp d l op r t = case (getAssgnType op) of
  'n' -> TacAssignBinOp d (makeTBinOp op t) l r (getTACType t)
  'b' -> TacAssignRelOp d (makeTRelOp op) l r (getTACType t)


-- Creates the right kind of operation
makeTBinOp :: Assignment_op a -> Type -> TBinOp
makeTBinOp op t = case op of
  AssgnAdd _ -> getOperator t Add
  AssgnSub _ -> getOperator t Sub
  AssgnMul _ -> getOperator t Mul
  AssgnDiv _ -> getOperator t Div
  AssgnPow _ -> getOperator t Pow


-- Creates the right kind of operation
makeTRelOp :: Assignment_op a -> TRelOp
makeTRelOp op = case op of
  AssgnAnd _ -> TACGen.And
  AssgnOr _ -> TACGen.Or


-- Given a counter , a list of Strings and a list of Stmt, returns a couple(list of Stmt TACS, updated counter)
getTACFromStmtList :: Integer -> [String] -> [Stmt TypeCheckRes] -> Lab -> ([Stmt TACS], Integer)
getTACFromStmtList c [] [] _ = ([], c)
getTACFromStmtList c (l:ls) (st:sts) nextWhile =
  let d = snd $ getTACFromStmt st c l nextWhile in
    
    ((fst $ getTACFromStmt st c l nextWhile):(fst $ getTACFromStmtList d ls sts nextWhile), snd $ getTACFromStmtList d ls sts nextWhile)


-- Given a list of Strings and a list of lists of TAC, returns a list of TAC
makeBlockCode :: [String] -> [TACS] -> TACS
makeBlockCode labs ts = mergeTACS (head ts) (makeBlockCodeAux labs $ tail ts)


-- Given a list of Strings and a list of lists of TAC, returns a list of TAC, creates a label for the string anc concatenates it with a TAC
makeBlockCodeAux :: [String] -> [TACS] -> TACS
makeBlockCodeAux [] [] = (TACS [] [] [])
makeBlockCodeAux (l:ls) (t:ts) = mergeTACS (t {text = ((TacLabel l):(text t))}) (makeBlockCodeAux ls ts)


-- Given two TACS a ne TACS where each element is the concatenaation of the elements of the two starting TACS
mergeTACS :: TACS -> TACS -> TACS
mergeTACS (TACS t1 f1 d1) (TACS t2 f2 d2) = TACS (t1 ++ t2) (f1 ++ f2) (d1 ++ d2)


preProc:: RExpr a -> Maybe Integer
preProc a = case rexprAddr a of
  CInt iVal -> Just iVal
  _ -> Nothing


--getPreprocOperation:: Op -> (Int -> Int)
getPreprocOperation Add = \x y -> x + y
getPreprocOperation Sub = \x y -> x - y
getPreprocOperation Mul = \x y -> x * y
getPreprocOperation Div = \x y -> x `div` y
getPreprocOperation AbsGrammar.Mod = \x y -> x `mod` y
getPreprocOperation Pow = \x y -> (^) x y


-- Given a RExpr, a counter, returns a couple (RExpr TACS, updated counter)
getValFromRExpr :: RExpr TypeCheckRes -> Integer -> (RExpr TACS, Integer)
getValFromRExpr rexpr c = case rexpr of
  
  AbsGrammar.Int _ _ int -> (AbsGrammar.Int (TACS [] [] []) (CInt $ myIntegerInt int) (int {myIntegerContent = (TACS [] [] [])}), c)
  AbsGrammar.Char _ _ char -> (AbsGrammar.Char (TACS [] [] []) (CChar $ myCharChar char) (char {myCharContent = (TACS [] [] [])}), c)
  AbsGrammar.Float _ _ double -> (AbsGrammar.Float (TACS [] [] []) (CDouble $ myDoubleDouble double) (double {myDoubleContent = (TACS [] [] [])}), c)
  AbsGrammar.String _ _ str ->
    let strpt = (newStr c) in
      (AbsGrammar.String (TACS [] [] [TacString (CString (myStringString str)) [strpt]]) strpt (str {myStringContent = (TACS [] [] [])}), c + 1)
  AbsGrammar.Bool _ _ bol -> case bol of
    Boolean_true _ -> (AbsGrammar.Bool (TACS [] [] []) (CBool True) (Boolean_true (TACS [] [] [])), c)
    Boolean_false _ -> (AbsGrammar.Bool (TACS [] [] []) (CBool False) (Boolean_false (TACS [] [] [])), c)

  AbsGrammar.BoolBinOp tres _ op l r -> 
    let newLT = newLab c in
    let newLF = newLab $ c + 1 in
    let newLE = newLab $ c + 2 in
    let newT = newTemp $ c + 3 in
    let rexpr = AbsGrammar.BoolBinOp tres (CAddr "") op l r in
    let tacjump = fst $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let d = snd $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let jtacs = (rexprContent tacjump) in
    let tacend = (TacLabel newLE):(TacCopy newT (CBool False) "boolean"):(TacLabel newLF):(TacJump newLE):(TacCopy newT (CBool True) "boolean"):(TacLabel newLT):[] in
    let cont = (jtacs {text = (tacend ++ (text jtacs))}) in
    
      (tacjump {rexprContent = cont, rexprAddr = newT}, d)

  AbsGrammar.Equality tres _ op l r ->
    let newLT = newLab c in
    let newLF = newLab $ c + 1 in
    let newLE = newLab $ c + 2 in
    let newT = newTemp $ c + 3 in
    let rexpr = AbsGrammar.Equality tres (CAddr "") op l r in
    let tacjump = fst $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let d = snd $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let jtacs = (rexprContent tacjump) in
    let tacend = (TacLabel newLE):(TacCopy newT (CBool False) "boolean"):(TacLabel newLF):(TacJump newLE):(TacCopy newT (CBool True) "boolean"):(TacLabel newLT):[] in
    let cont = (jtacs {text = (tacend ++ (text jtacs))}) in
    
      (tacjump {rexprContent = cont, rexprAddr = newT}, d)

  AbsGrammar.Comparison tres _ op l r ->
    let newLT = newLab c in
    let newLF = newLab $ c + 1 in
    let newLE = newLab $ c + 2 in
    let newT = newTemp $ c + 3 in
    let rexpr = AbsGrammar.Comparison tres (CAddr "") op l r in
    let tacjump = fst $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let d = snd $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let jtacs = (rexprContent tacjump) in
    let tacend = (TacLabel newLE):(TacCopy newT (CBool False) "boolean"):(TacLabel newLF):(TacJump newLE):(TacCopy newT (CBool True) "boolean"):(TacLabel newLT):[] in
    let cont = (jtacs {text = (tacend ++ (text jtacs))}) in

      (tacjump {rexprContent = cont, rexprAddr = newT}, d)

  AbsGrammar.Not tres _ ex ->
    let newLT = newLab c in
    let newLF = newLab $ c + 1 in
    let newLE = newLab $ c + 2 in
    let newT = newTemp $ c + 3 in
    let rexpr = AbsGrammar.Not tres (CAddr "") ex in
    let tacjump = fst $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let d = snd $ getTACFromRExpr rexpr (c + 4) newLT newLF in
    let jtacs = (rexprContent tacjump) in
    let tacend = (TacLabel newLE):(TacCopy newT (CBool False) "boolean"):(TacLabel newLF):(TacJump newLE):(TacCopy newT (CBool True) "boolean"):(TacLabel newLT):[] in
    let cont = (jtacs {text = (tacend ++ (text jtacs))}) in
    
      (tacjump {rexprContent = cont, rexprAddr = newT}, d)

  AbsGrammar.Ref _ _ lexpr ->
    let tacl = fst $ getTACFromLExpr lexpr c in
    let addr = newTemp c in
      (AbsGrammar.Ref (TACS ((TacAssignRef addr $ lexprAddr tacl):(text (lexprContent tacl))) [] (dataMem (lexprContent tacl))) addr tacl, c + 1)
  
  AbsGrammar.Neg tres _ r ->
    let d = snd $ getValFromRExpr r c in
    let newT = newTemp d in
    let tacrexpr = fst $ getValFromRExpr r c in
    let rtacs = (rexprContent tacrexpr) in
    
      (AbsGrammar.Neg (rtacs {text = ((TacAssignUnOp newT TACGen.Neg (rexprAddr tacrexpr) (getTACType $ getType tres)):(text rtacs))}) newT tacrexpr, d + 1)
  
  AbsGrammar.Lexpr _ _ lexpr ->
    let tacl = fst $ getTACFromLExpr lexpr c in
    let d = snd $ getTACFromLExpr lexpr c in

      (AbsGrammar.Lexpr (lexprContent tacl) (lexprAddr tacl) tacl, d)
  
  AbsGrammar.BinOp typ k op l r ->
    let preProcOp = getPreprocOperation op in
    let d = snd $ getValFromRExpr l c in
    let e = snd $ getValFromRExpr r d in
    let newT = newTemp e in
    let lType = getType $ rexprContent l in
    let rType = getType $ rexprContent r in
    let tacl = fst $ getValFromRExpr l c in
    let tacr = fst $ getValFromRExpr r d in
    let tacexprs = (text (rexprContent tacr)) ++ (text (rexprContent tacl)) in
    let strs = ((dataMem (rexprContent tacr)) ++ (dataMem (rexprContent tacl))) in
        
        if lType == rType
          then if (isJust $ preProc tacl) && (isJust $ preProc tacr) -- preprocessing case
            then (Int (TACS [] [] []) (CInt $ preProcOp (fromJust $ preProc tacl) (fromJust $ preProc tacr)) (MyInteger (TACS [] [] []) (preProcOp (fromJust $ preProc tacl) (fromJust $ preProc tacr))), c)
            else (AbsGrammar.BinOp (TACS ((TacAssignBinOp newT (getOperator lType op) (rexprAddr tacl) (rexprAddr tacr) (getTACType lType)):tacexprs) [] strs) newT op tacl tacr, e + 1)
            
          else -- NOT preprocessing case
            let sup = superType lType rType in
            let newTmp1 = newTemp $ e + 1 in
              if lType == sup
                then (AbsGrammar.BinOp (TACS ((TacAssignBinOp newT (getOperator sup op) (rexprAddr tacl) newTmp1 (getTACType sup)) : (cast newTmp1 (rexprAddr tacr) sup rType) : (text $ rexprContent tacr) ++ (text $ rexprContent tacl)) [] []) newT op tacl tacr, e + 2)
                else (AbsGrammar.BinOp (TACS ((TacAssignBinOp newT (getOperator sup op) newTmp1 (rexprAddr tacr) (getTACType sup)) : (cast newTmp1 (rexprAddr tacl) sup lType) : (text $ rexprContent tacr) ++ (text $ rexprContent tacl) ) [] [] )newT op tacl tacr, e + 2)

  AbsGrammar.FCall ty addr id params -> getTACFromFCall ty addr (identString id) params c
  
  AbsGrammar.IfRe ty _ guard the els ->
    let newLF = newLab c in
    let newLF2 = newLab (c+1) in
    let newT = newTemp (c+2) in
    let d = getTACFromRExpr guard (c + 3) fall newLF in
    let e = getValFromRExpr the (snd d) in
    let f = getValFromRExpr els (snd e) in
    let sup = superType (getType $ rexprContent the) (getType $ rexprContent els) in
    let te = if((getType $ rexprContent the) == sup && (getType $ rexprContent els) == sup) 
               then [TacLabel newLF2] ++ [TacCopy newT (rexprAddr (fst f)) (getTACType sup)] ++ (text(rexprContent $ fst f)) ++ [TacLabel newLF] ++ [TacJump newLF2] ++[TacCopy newT (rexprAddr (fst e)) (getTACType sup)] ++ (text(rexprContent $ fst e)) ++ (text(rexprContent $ fst d))
               
               else if((getType $ rexprContent the) == sup && (getType $ rexprContent els) /= sup)
                 then [TacLabel newLF2] ++ [TacAssignUnOp newT IntToFloat (rexprAddr (fst f)) (getTACType sup)] ++ (text(rexprContent $ fst f)) ++ [TacLabel newLF] ++ [TacJump newLF2] ++[TacCopy newT (rexprAddr (fst e)) (getTACType sup)] ++ (text(rexprContent $ fst e)) ++ (text(rexprContent $ fst d))
                 
                 else [TacLabel newLF2] ++ [TacCopy newT (rexprAddr (fst f)) (getTACType sup)] ++ (text(rexprContent $ fst f)) ++ [TacLabel newLF] ++ [TacJump newLF2] ++[TacAssignUnOp newT IntToFloat (rexprAddr (fst e)) (getTACType sup)] ++ (text(rexprContent $ fst e)) ++ (text(rexprContent $ fst d)) in

    let st = ((dataMem (rexprContent $ fst f)) ++ (dataMem (rexprContent $ fst e)) ++ (dataMem(rexprContent $ fst d))) in
    
      (AbsGrammar.IfRe (TACS te [] st) newT (fst d) (fst e) (fst f), snd f)

  AbsGrammar.PreCalc _ res ->
    (AbsGrammar.PreCalc (TACS [] [] []) res, c)

   
     
-- Given a RExpr, a counter, the true label and the false label, returns a couple (RExpr TACS, updated counter)
--it is used for RExprs contained int the guard of a if or while
getTACFromRExpr :: RExpr TypeCheckRes -> Integer -> Lab -> Lab -> (RExpr TACS, Integer)
getTACFromRExpr rexpr c tl fl = case rexpr of

  AbsGrammar.BoolBinOp typ _ op l r ->
    let newL = newLab c in
      case op of
        
        AbsGrammar.And ->
          let falseL = if (isFall fl) then newL else fl in
          let labtac = if (isFall fl) then [(TacLabel newL)] else [] in
          let tacl = fst $ getTACFromRExpr l (c + 1) fall falseL in
          let d = snd $ getTACFromRExpr l (c + 1) fall falseL in
          let ltacs = (rexprContent tacl) in
          let tacr = fst $ getTACFromRExpr r d tl fl in
          let e = snd $ getTACFromRExpr r d tl fl in
          let rtacs = (rexprContent tacr) in
            
            (AbsGrammar.BoolBinOp (TACS (labtac ++ (text rtacs) ++ (text ltacs)) [] ((dataMem ltacs) ++ (dataMem rtacs))) (CString "") op tacl tacr, e)

        AbsGrammar.Or ->
          let trueL = if (isFall tl) then newL else tl in
          let labtac = if (isFall tl) then [(TacLabel newL)] else [] in
          let tacl = fst $ getTACFromRExpr l (c + 1) trueL fall in
          let d = snd $ getTACFromRExpr l (c + 1) trueL fall in
          let ltacs = (rexprContent tacl) in
          let tacr = fst $ getTACFromRExpr r d tl fl in
          let e = snd $ getTACFromRExpr r d tl fl in
          let rtacs = (rexprContent tacr) in
            
            (AbsGrammar.BoolBinOp (TACS (labtac ++ (text rtacs) ++ (text ltacs)) [] ((dataMem ltacs) ++ (dataMem rtacs))) (CString "") op tacl tacr, e)

  AbsGrammar.Equality typ ad op l r -> case (getType (rexprContent l)) of
    SimpTyp T_String ->
      let newT = (newTemp c) in
      let compCall = (AbsGrammar.FCall (typ {getType = (SimpTyp T_Int), getSize = dimInt}) (CString "") (Ident uselessTypeCheckRes compStrCall) [l,r]) in
      let tacall = (fst $ getValFromRExpr compCall (c+1)) in
      let d = (snd $ getValFromRExpr compCall (c+1)) in
      let tacop = (getEqOperatorType (SimpTyp T_Int) op) in
      let tacjumps = case (isFall tl, isFall fl) of
           (False, False) -> (TacJump fl) : [TacRelConJump tl True tacop (rexprAddr tacall) (CInt 0)]
           (True, False) -> [TacRelConJump fl False tacop (rexprAddr tacall) (CInt 0)]
           (False, True) -> [TacRelConJump tl True tacop (rexprAddr tacall) (CInt 0)] in
      let ctacs = (rexprContent tacall) in
      let cont = (ctacs {text = (tacjumps ++ (text ctacs))}) in
        (AbsGrammar.Equality cont (CString "") op (head (rexprParams tacall)) (head (tail (rexprParams tacall))), d)

    _ ->
      let d = snd $ getValFromRExpr l c in
      let e = snd $ getValFromRExpr r d in
      let tempo1 = newTemp e in
      let tempo2 = newTemp (e+1) in
      let tacl = fst $ getValFromRExpr l c in
      let tacr = fst $ getValFromRExpr r d in
      let ltacs = (rexprContent tacl) in
      let rtacs = (rexprContent tacr) in
      let lType = getType $ rexprContent l in
      let rType = getType $ rexprContent r in
      let sup = if(lType==rType) then lType else superType lType rType in
      let addL = if (lType == rType) || (lType == sup) then rexprAddr tacl else getAddrResult(cast tempo1 (rexprAddr tacl) sup lType) in
      let addR = if (lType == rType) || (rType == sup) then rexprAddr tacr else getAddrResult(cast tempo2 (rexprAddr tacr) sup rType) in
      let tacjumps = case (isFall tl, isFall fl) of
            (False, False) -> (TacJump fl) : [TacRelConJump tl True (getEqOperatorType sup op) addL addR]
            (True, False) -> [TacRelConJump fl False (getEqOperatorType sup op) addL addR]
            (False, True) -> [TacRelConJump tl True (getEqOperatorType sup op) addL addR] in
      let castac = if (lType==rType) then [] else if (lType /= sup) then [cast tempo1 (rexprAddr tacl) sup lType] else [cast tempo2 (rexprAddr tacr) sup rType] in

        (AbsGrammar.Equality (TACS (tacjumps ++ castac ++(text rtacs) ++ (text ltacs)) [] ((dataMem ltacs) ++ (dataMem rtacs))) (CString "") op tacl tacr, (e+2))


  AbsGrammar.Comparison typ ad op l r -> case (getType (rexprContent l)) of
    SimpTyp T_String ->
      let newT = (newTemp c) in
      let compCall = (AbsGrammar.FCall (typ {getType = (SimpTyp T_Int), getSize = dimInt}) (CString "") (Ident uselessTypeCheckRes compStrCall) [l,r]) in
      let tacall = (fst $ getValFromRExpr compCall (c+1)) in
      let d = (snd $ getValFromRExpr compCall (c+1)) in
      let tacop = (getCoOperatorType (SimpTyp T_Int) op) in
      let tacjumps = case (isFall tl, isFall fl) of
            (False, False) -> (TacJump fl) : [TacRelConJump tl True tacop (rexprAddr tacall) (CInt 0)]
            (True, False) -> [TacRelConJump fl False tacop (rexprAddr tacall) (CInt 0)]
            (False, True) -> [TacRelConJump tl True tacop (rexprAddr tacall) (CInt 0)] in
      let ctacs = (rexprContent tacall) in
      let cont = (ctacs {text = (tacjumps ++ (text ctacs))}) in
        (AbsGrammar.Comparison cont (CString "") op (head (rexprParams tacall)) (head (tail (rexprParams tacall))), d)

    _ ->
      let d = snd $ getValFromRExpr l c in
      let e = snd $ getValFromRExpr r d in
      let tempo1 = newTemp e in
      let tempo2 = newTemp (e+1) in
      let tacl = fst $ getValFromRExpr l c in
      let tacr = fst $ getValFromRExpr r d in
      let ltacs = (rexprContent tacl) in
      let rtacs = (rexprContent tacr) in
      let lType = getType $ rexprContent l in
      let rType = getType $ rexprContent r in
      let sup = if(lType==rType) then lType else superType lType rType in
      let addL = if (lType == rType) || (lType == sup) then rexprAddr tacl else getAddrResult(cast tempo1 (rexprAddr tacl) sup lType) in
      let addR = if (lType == rType) || (rType == sup) then rexprAddr tacr else getAddrResult(cast tempo2 (rexprAddr tacr) sup rType) in
      let tacjumps = case (isFall tl, isFall fl) of
            (False, False) -> (TacJump fl) : [TacRelConJump tl True (getCoOperatorType sup op) addL addR]
            (True, False) -> [TacRelConJump fl False (getCoOperatorType sup op) addL addR]
            (False, True) -> [TacRelConJump tl True (getCoOperatorType sup op) addL addR] in
      let castac = if (lType==rType) then [] else if (lType /= sup) then [cast tempo1 (rexprAddr tacl) sup lType] else [cast tempo2 (rexprAddr tacr) sup rType] in

        (AbsGrammar.Comparison (TACS (tacjumps ++ castac ++(text rtacs) ++ (text ltacs)) [] ((dataMem ltacs) ++ (dataMem rtacs))) (CString "") op tacl tacr, (e+2))

  AbsGrammar.Bool _ _ bol -> case bol of
    
    Boolean_true _ ->
      let tacjump = if (isFall tl) then [] else [TacJump tl] in
        (AbsGrammar.Bool (TACS tacjump [] []) (CString "") (Boolean_true (TACS [] [] [])), c)
    
    Boolean_false _ ->
      let tacjump = if (isFall fl) then [] else [TacJump fl] in
        (AbsGrammar.Bool (TACS tacjump [] []) (CString "") (Boolean_false (TACS [] [] [])), c)

  AbsGrammar.Not _ _ r ->
    let tacr = fst $ getTACFromRExpr r c fl tl in
      (AbsGrammar.Not (rexprContent tacr) (CString "") tacr, c)

  AbsGrammar.PreCalc _ res ->
    let tacjumps = case (isFall tl, isFall fl) of
          (False, False) -> [TacJump fl, TacConJump tl True res]
          (True, False) -> [TacConJump fl False res]
          (False, True) -> [TacConJump tl True res] in

      (AbsGrammar.PreCalc (TACS tacjumps [] []) (CAddr ""), c)

  AbsGrammar.Lexpr _ _ lexpr ->
    let tacl = fst $ getTACFromLExpr lexpr c in
    let d = snd $ getTACFromLExpr lexpr c in
    let ltacs = (lexprContent tacl) in
    let tacjumps = case (isFall tl, isFall fl) of
          (False, False) -> [TacJump fl, TacConJump tl True (lexprAddr tacl)]
          (True, False) -> [TacConJump fl False (lexprAddr tacl)]
          (False, True) -> [TacConJump tl True (lexprAddr tacl)] in
    
      (AbsGrammar.Lexpr (ltacs {text = (tacjumps ++ (text ltacs))}) (CAddr "") tacl, d)

  AbsGrammar.FCall tres addr id pars ->
    let fcall = (AbsGrammar.FCall tres addr id pars) in
    let val = (fst $ getValFromRExpr fcall c) in
    let d = (snd $ getValFromRExpr fcall c) in
    let rtacs = (rexprContent val) in
    let tacjumps = case (isFall tl, isFall fl) of
          (False, False) -> [TacJump fl, TacConJump tl True (rexprAddr val)]
          (True, False) -> [TacConJump fl False (rexprAddr val)]
          (False, True) -> [TacConJump tl True (rexprAddr val)] in
    let cont = (rtacs {text = (tacjumps ++ (text rtacs))}) in

      (val {rexprContent = cont, rexprAddr = (CAddr "")}, d)

  AbsGrammar.IfRe ty _ guard the els ->
    let newL = newLab c in
    let endL = newLab (c+1) in
    let d = getTACFromRExpr guard (c + 1) fall newL in
    let e = getTACFromRExpr the (snd d) tl fl in
    let f = getTACFromRExpr els (snd e) tl fl in
    let te = ([TacLabel endL] ++ text (rexprContent (fst f))) ++ [TacLabel newL, TacJump endL] ++ (text (rexprContent (fst e))) ++ (text (rexprContent (fst d))) in
    let st = ((dataMem (rexprContent $ fst f)) ++ (dataMem (rexprContent $ fst e)) ++ (dataMem(rexprContent $ fst d))) in

      (AbsGrammar.IfRe (TACS te [] st) (CAddr "") (fst d) (fst e) (fst f), snd f)



isFall :: Lab -> Bool
isFall "Fall" = True
isFall l = False


fall :: Lab
fall = "Fall"

compStrCall :: String
compStrCall = "mine$strcmp"

-- Given a TypeCheckRes, an Addr where to put the result of the function call, the name of the function, a list of RExpr and the counter, returns a couple(Stmt TACS , updated counter)
getTACFromProcCall:: TypeCheckRes -> Addr -> String -> [RExpr TypeCheckRes] -> Integer -> (Stmt TACS, Integer)
getTACFromProcCall ty addr id params c =
  let parTypes = Prelude.map (getType . rexprContent) params in
  let funcToCall = head $ Prelude.filter (TACGen.getCorrectFunctionToCall parTypes id $ getEnv ty) (findWithDefault [] id $ getEnv ty) in
  let paramsTac = TACGen.getTACFromCallParams funcToCall params c in
  let parNum = (fromIntegral (getTACParLength (funParams funcToCall))) in
    
    (ProcCall (TACS ((TacProcCall (getIdentifier (funPos funcToCall) id) parNum):(text (fst paramsTac))) [] (dataMem (fst paramsTac))) (Ident (TACS [] [] []) id) [] , snd paramsTac)


-- Given a TypeCheckRes, an Addr where to put the result of the function call, the name of the function, a list of RExpr and the counter, returns a couple(RExpr TACS , updated counter)
getTACFromFCall:: TypeCheckRes -> Addr -> String -> [RExpr TypeCheckRes] -> Integer -> (RExpr TACS, Integer)
getTACFromFCall ty addr id params c =
  let parTypes = Prelude.map (getType . rexprContent) params in
  let funcToCall = head $ Prelude.filter (TACGen.getCorrectFunctionToCall parTypes id $ getEnv ty) (findWithDefault [] id $ getEnv ty) in
  let paramsTac = TACGen.getTACFromCallParams funcToCall params c in
  let parNum = (fromIntegral (getTACParLength (funParams funcToCall))) in
  
    (FCall (TACS ((TacFunCall (getIdentifier (funPos funcToCall) id) parNum (newTemp $ snd paramsTac) (getTACType $ getType ty)):(text (fst paramsTac))) [] (dataMem (fst paramsTac))) (newTemp $ snd paramsTac) (Ident (TACS [] [] []) id) [] , (+) 1 $ snd paramsTac)


getTACParLength :: [Param] -> Integer
getTACParLength [] = 0
getTACParLength (p:ps) = case p of
  TypeChecking.Param (Type.Array te len se) pos mod str size ->
        if isValMod mod
          then
            (getTotLen (Type.Array te len se)) `div` (getTypeSize (getBaseType (Type.Array te len se))) + (getTACParLength ps)
          else
            1 + (getTACParLength ps)
  _ -> 1 + (getTACParLength ps)


isValMod :: AbsGrammar.Modality -> Bool
isValMod mod = mod == Modality1 || mod == Modality_val || mod == Modality_const

-- Given an EnvEntry, a list of RExpr, the counter, two labels, returns a couple(list of TAC, updated counter)
getTACFromCallParams:: EnvEntry -> [RExpr TypeCheckRes] -> Integer -> (TACS, Integer)
getTACFromCallParams fun rexpr c = getTACFromCallParamsAux (funParams fun) rexpr c


mountComplex :: (RExpr TypeCheckRes, Bool) -> ComplexRExpr TypeCheckRes
mountComplex (rexpr,True) = mountComplex ((rebuildWithRef rexpr), False)
mountComplex (rexpr,False) = AbsGrammar.Simple (rexprContent rexpr) (CString "") rexpr


rebuildWithRef :: RExpr TypeCheckRes -> RExpr TypeCheckRes
rebuildWithRef (Lexpr tres a l) = Ref (tres {getType = (Point (getType tres) (getSize tres)), getSize = dimPointer}) (CString "") l


-- Given an EnvEntry, a list of RExpr, the counter, two labels, returns a couple(list of TAC, updated counter), makes the cast for the params that need it
getTACFromCallParamsAux:: [Param] -> [RExpr TypeCheckRes] -> Integer -> (TACS, Integer)
getTACFromCallParamsAux pars rexprs c =
  let points = Prelude.map (\x -> not (isValMod (getParamModality x))) pars in
  let comps = Prelude.map mountComplex (zip rexprs points) in
  let tacomps = (fst $ getTACFromComplexRExprList c comps) in
  let d = (snd $ getTACFromComplexRExprList c comps) in
  let types = Prelude.map (getType . rexprContent) rexprs in
    attachPars (reverse pars) (reverse tacomps) (reverse types) (reverse points) d


attachPars :: [Param] -> [ComplexRExpr TACS] -> [Type] -> [Bool] -> Integer -> (TACS, Integer)
attachPars [] [] [] [] c = ((TACS [] [] []), c)
attachPars (p:ps) (r:rs) (t:ts) (b:bs) c =
  let castDest = (newTemp c) in
  let tp = (getParamType p) in
  let restyp = if b then (Point (SimpTyp T_Void) dimVoid) else tp in
  let addr = (complexRexprAddr r) in
  let needCast = tp == t in
  let castac = if needCast then [] else [cast castDest addr tp t] in
  let d = if needCast then c+1 else c in
  let last = if needCast then addr else castDest in
    case p of
      TypeChecking.Param (Type.Array te len se) pos mod str size ->
        if isValMod mod
          then
            let att = (fst $ makeArrayReads (complexRexprAddr r) (getBaseType (Type.Array te len se)) (getBaseType t) (getTotLen (Type.Array te len se)) d) in
            let e =   (snd $ makeArrayReads (complexRexprAddr r) (getBaseType (Type.Array te len se)) (getBaseType t) (getTotLen (Type.Array te len se)) d) in
            let rec = (attachPars ps rs ts bs e) in
              (mergeTACS att (mergeTACS (complexRexprContent r) (fst rec)), snd rec)
          else
            let rec = (attachPars ps rs ts bs d) in
              (mergeTACS (TACS ([TacParam last (getTACType restyp)] ++ castac) [] []) (mergeTACS (complexRexprContent r) (fst rec)), snd rec)
      _ ->
        let rec = (attachPars ps rs ts bs d) in
          (mergeTACS (TACS ([TacParam last (getTACType restyp)] ++ castac) [] []) (mergeTACS (complexRexprContent r) (fst rec)), snd rec)


getTotLen :: Type -> Integer
getTotLen t = case t of
  Type.Array te len _ -> len * (getTotLen te)
  el -> getTypeSize el


makeArrayReads :: Addr -> Type -> Type ->Integer -> Integer -> (TACS, Integer)
makeArrayReads base ta tr 0 c = (TACS [] [] [], c)

makeArrayReads base ta tr off c =
  let readDest = (newTemp c) in
  let castDest = (newTemp (c+1)) in
  let inc = (getTypeSize ta) in
  let castac = if ta == tr then [] else [cast castDest readDest ta tr] in
  let d = if ta == tr then (c+1) else (c+2) in
  let last = if ta == tr then readDest else castDest in
  let rec = (makeArrayReads base ta tr (off-inc) d) in
    (mergeTACS (TACS ([TacParam last (getTACType ta)] ++ castac ++ [TacArrayRead readDest base (CInt (off - inc)) (getTACType tr)]) [] []) (fst rec), snd rec)


-- Given the list of the types of the function parameters, the name of the function, the enviroment and a entry of that enviroment, returns true if all the parameters are compliant with the function definition, false otherwise
getCorrectFunctionToCall:: [Type] -> String -> Env -> EnvEntry -> Bool
getCorrectFunctionToCall aParams fName env (Fun fPos fParams fTy fDim) =
  TACGen.paramsCompliantCheck aParams fParams fName


-- Checks if the params in a function call are compliant with those in the funcion definition
paramsCompliantCheck :: [Type] -> [Param] -> String -> Bool
paramsCompliantCheck [] [] _ = True
paramsCompliantCheck aParams fParams fName | (length aParams) /= (length fParams) = False
paramsCompliantCheck aParams fParams fName =
  if subType (head aParams) (getParamType $ head fParams)
    then True && (TACGen.paramsCompliantCheck (tail aParams) (tail fParams) fName)
    else False


-- Given a VarDeclInit and a counter, returns a couple (VarDeclInit TACS, updated counter)
getTACFromVarDeclInit :: VarDeclInit TypeCheckRes -> Integer -> (VarDeclInit TACS, Integer)
getTACFromVarDeclInit decl c = case decl of
  VarDeclIn tres _ (Ident ti id) typ (AbsGrammar.Array ta ad comps) check ->
    let rexpr = (AbsGrammar.Array ta ad comps) in
    let addr = getIdentifier (getPos ti) id in
    let tacr = fst $ getTACFromComplexRExpr rexpr c in
    let d = snd $ getTACFromComplexRExpr rexpr c in
    let bt = (getBaseType (getType tres)) in
    let off = ((getSize  tres) - (getTypeSize bt)) in
    let taclist = reverse $ Prelude.map complexRexprContent $ complexRexprListRexpr tacr in
      case complexRexprAddr tacr of
        CList els typs ->
          let assigns = makeArrayAssign addr bt (reverse els) (reverse typs) off d taclist in
            (VarDeclIn (fst assigns) addr (Ident (TACS [] [] []) id) (getTACFromTypeSpec typ) tacr check, snd assigns)

  ConDeclIn tres _ (Ident ti id) typ (AbsGrammar.Array ta ad comps) check ->
    let rexpr = (AbsGrammar.Array ta ad comps) in
    let addr = getIdentifier (getPos ti) id in
    let tacr = fst $ getTACFromComplexRExpr rexpr c in
    let d = snd $ getTACFromComplexRExpr rexpr c in
    let bt = (getBaseType (getType tres)) in
    let off = ((getSize  tres) - (getTypeSize bt)) in
    let taclist = reverse $ Prelude.map complexRexprContent $ complexRexprListRexpr tacr in
      case complexRexprAddr tacr of
        CList els typs ->
          let assigns = makeArrayAssign addr bt (reverse els) (reverse typs) off d taclist in
             (ConDeclIn (fst assigns) addr (Ident (TACS [] [] []) id) (getTACFromTypeSpec typ) tacr check, snd assigns)

  VarDeclIn tres _ (Ident ti id) typ rexpr check ->
    let addr = getIdentifier (getPos ti) id in
    let tacr = fst $ getTACFromComplexRExpr rexpr c in
    let d = snd $ getTACFromComplexRExpr rexpr c in
    let rtacs = (complexRexprContent tacr) in
    let ta = getType tres in
    let te = getType $ complexRexprContent rexpr in
        
      if ta == te
        then (VarDeclIn (rtacs {text = ((TacCopy addr (complexRexprAddr tacr) (getTACType ta)):(text rtacs))}) addr (Ident (TACS [] [] []) id) (getTACFromTypeSpec typ) tacr check, d)
        else (VarDeclIn (rtacs {text = ((cast addr (complexRexprAddr tacr) ta te):(text rtacs))}) addr (Ident (TACS [] [] []) id) (getTACFromTypeSpec typ) tacr check, d)

  ConDeclIn tres _ (Ident ti id) typ rexpr check ->
    let addr = getIdentifier (getPos ti) id in
    let tacr = fst $ getTACFromComplexRExpr rexpr c in
    let d = snd $ getTACFromComplexRExpr rexpr c in
    let rtacs = (complexRexprContent tacr) in
    let ta = getType tres in
    let te = getType $ complexRexprContent rexpr in
        
      if ta == te
        then (ConDeclIn (rtacs {text = ((TacCopy addr (complexRexprAddr tacr) (getTACType ta)):(text rtacs))}) addr (Ident (TACS [] [] []) id) (getTACFromTypeSpec typ) tacr check, d)
        else (ConDeclIn (rtacs {text = ((cast addr (complexRexprAddr tacr) ta te):(text rtacs))}) addr (Ident (TACS [] [] []) id) (getTACFromTypeSpec typ) tacr check, d)


makeArrayAssign :: Addr -> Type -> [Addr] -> [Type] -> Integer -> Integer -> [TACS] -> (TACS, Integer)
makeArrayAssign id bt [] [] off c [] = ((TACS [] [] []), c)
makeArrayAssign id bt (a:as) (t:ts) off c (l:ls) = let inc = (getTypeSize bt) in
  let assignStep = (mergeTACS (l {text = ((TacArrayWrite id (CInt off) a (getTACType bt):(text l)))}) (fst $ makeArrayAssign id bt as ts (off - inc) c ls), (snd $ makeArrayAssign id bt as ts (off - inc) c ls)) in
    case a of
      CList addrs types -> let len = ((length addrs) - 1)  in
        makeArrayAssign id bt ((reverse addrs) ++ as) ((reverse types) ++ ts) off c ((emptyLists len) ++ (l:ls))

      CInt num -> case bt of
        SimpTyp T_Float64 -> makeArrayAssign id bt ((CDouble (fromIntegral num)):as) ((SimpTyp T_Float64):ts) off c (l:ls)
        _ -> assignStep

      _ -> case bt /= t of
        True -> let newT = (newTemp c) in
          (mergeTACS (l {text = ((TacArrayWrite id (CInt off) newT (getTACType bt)):(cast newT a bt t):(text l))}) (fst $ makeArrayAssign id bt as ts (off - inc) (c+1) ls), (snd $ makeArrayAssign id bt as ts (off - inc) (c+1) ls))
        False -> assignStep


-- Given an integer n it creates a list of n empty TACS
emptyLists :: Int -> [TACS]
emptyLists 0 = []
emptyLists n = (TACS [] [] []):(emptyLists (n-1))



-- Given an array returns the type of the elements of that array
getBaseType :: Type -> Type
getBaseType t = case t of
  Type.Array te _ _ -> getBaseType te
  _ -> t


-- Given a ComplexRExpr and a counter, returns a couple (ComplexRExpr TACS, updated counter)
getTACFromComplexRExpr :: ComplexRExpr TypeCheckRes -> Integer -> (ComplexRExpr TACS, Integer)
getTACFromComplexRExpr comp c = case comp of
  AbsGrammar.Array tres _ comps ->
    let tacomps = (fst $ getTACFromComplexRExprList c comps) in
    let d = (snd $ getTACFromComplexRExprList c comps) in
    let addr = (CList (Prelude.map complexRexprAddr tacomps) (Prelude.map (getType . complexRexprContent) comps)) in
    let tac = (Prelude.foldl (\x y -> mergeTACS y x) (TACS [] [] []) (Prelude.map complexRexprContent tacomps)) in
      (AbsGrammar.Array tac addr tacomps, d)
  
  AbsGrammar.Simple _ _ rexpr -> 
    let tacr = fst $ getValFromRExpr rexpr c in
    let d = snd $ getValFromRExpr rexpr c in
    (AbsGrammar.Simple (rexprContent tacr) (rexprAddr tacr) tacr, d)


getTACFromComplexRExprList :: Integer -> [ComplexRExpr TypeCheckRes] -> ([ComplexRExpr TACS], Integer)
getTACFromComplexRExprList c [] = ([], c)
getTACFromComplexRExprList c (r:rs) =
  let d = snd $ getTACFromComplexRExpr r c in
    ((fst $ getTACFromComplexRExpr r c):(fst $ getTACFromComplexRExprList d rs), snd $ getTACFromComplexRExprList d rs)


-- Given a TypeSpec, a counter and the label next, returns a couple (TypeSpec TACS, updated counter)
getTACFromTypeSpec :: TypeSpec TypeCheckRes -> TypeSpec TACS
getTACFromTypeSpec typ = case typ of
  BasTyp _ b -> BasTyp (TACS [] [] []) $ b {basicTypeContent = (TACS [] [] [])}
  ArrDef _ t i -> ArrDef (TACS [] [] []) (getTACFromTypeSpec t) $ i {myIntegerContent = (TACS [] [] [])}
  ArrUnDef _ t -> ArrUnDef (TACS [] [] []) (getTACFromTypeSpec t)
  Pointer _ t -> Pointer (TACS [] [] []) (getTACFromTypeSpec t)


-- Given a SelectionStmt, a counter and the label next, returns a couple (SelectionStmt TACS, updated counter)
getTACFromSelectionStmt:: SelectionStmt TypeCheckRes -> Integer -> Lab -> Lab -> (SelectionStmt TACS, Integer)
getTACFromSelectionStmt typ c next nextWhile= case typ of

  AbsGrammar.IfNoElse _ guard block ->
    let tacg = fst $ getTACFromRExpr guard c fall next in
    let d = snd $ getTACFromRExpr guard c fall next in
    let tacb = fst $ getTACFromBlockDecl block d next nextWhile in
    let e = snd $ getTACFromBlockDecl block d next nextWhile in
    let dat = ((dataMem(blockDeclContent tacb)) ++ (dataMem (rexprContent tacg))) in

      (AbsGrammar.IfNoElse (TACS ((text (blockDeclContent tacb)) ++ (text (rexprContent tacg))) (functions (blockDeclContent tacb)) ((dataMem (blockDeclContent tacb)) ++ (dataMem (rexprContent tacg)))) tacg tacb, e)

  AbsGrammar.IfElse _ guard the els ->
    let newLF = newLab c in
    let d = getTACFromRExpr guard (c + 1) fall newLF in
    let e = getTACFromBlockDecl the (snd d) next nextWhile in
    let f = getTACFromBlockDecl els (snd e) next nextWhile in
    let te = (text(blockDeclContent $ fst f)) ++ [TacLabel newLF] ++ [TacJump next] ++ (text(blockDeclContent $ fst e)) ++ (text(rexprContent $ fst d)) in
    let fu = ((functions (blockDeclContent $ fst f)) ++ (functions (blockDeclContent $ fst e)))  in 
    let st = ((dataMem (blockDeclContent $ fst f)) ++ (dataMem (blockDeclContent $ fst e)) ++ (dataMem(rexprContent $ fst d))) in

      (AbsGrammar.IfElse (TACS te fu st) (fst d) (fst e) (fst f), snd f)


getAssignTACFromLExpr :: LExpr TypeCheckRes -> Integer -> Addr -> (LExpr TACS, Integer)
getAssignTACFromLExpr lexpr c source = case lexpr of
  AbsGrammar.BasLExpr _ _ blexpr ->
    let tacl = fst $ getAssignTACFromBLExpr blexpr c source in
    let d = snd $ getAssignTACFromBLExpr blexpr c source in
      (AbsGrammar.BasLExpr ( TACS (text(blexprContent tacl)) (functions(blexprContent tacl)) (dataMem(blexprContent tacl)) ) (blexprAddr tacl) tacl, d)

  AbsGrammar.Deref tres _ rexpr ->
    let tacr = (fst $ getValFromRExpr rexpr c) in
    let d = (snd $ getValFromRExpr rexpr c) in
    let typ = (getType tres) in
    let point = (rexprAddr tacr) in
      (AbsGrammar.Deref ((TACS ((TacWritePointee point source (getTACType typ)):(text(rexprContent tacr)))) [] (dataMem(rexprContent tacr)) ) source tacr, d)

  AbsGrammar.IncDec _ _ _ l -> getAssignTACFromLExpr l c source


getAssignTACFromBLExpr :: BLExpr TypeCheckRes -> Integer -> Addr -> (BLExpr TACS, Integer)
getAssignTACFromBLExpr blexpr c source = case blexpr of

  AbsGrammar.ParLExpr trs _ _ lexpr ->
    let tacl = (fst $ getAssignTACFromLExpr lexpr c source) in
    let d = (snd $ getAssignTACFromLExpr lexpr c source) in

      (AbsGrammar.ParLExpr (lexprContent tacl) (lexprAddr tacl) (CAddr "") tacl, d)

  AbsGrammar.Id tres _ _ str ->
    let env = getEnv tres in
    let entry = head $ findWithDefault [] str env in
    let pos = (varPos entry) in
    let mod = (varMod entry) in
    let addr = if mod == Modality_valres then (CAddr ((show (getIdentifier pos str)) ++ "$valres")) else (getIdentifier pos str) in
    let typ = getType tres in
    let tac = if mod == Modality_ref then (TacWritePointee addr source (getTACType typ)) else (TacCopy addr source (getTACType typ)) in
      (AbsGrammar.Id (TACS [tac] [] []) (getIdentifier pos str) (CAddr "") str, c)

  AbsGrammar.ArrayEl con a ar l r ->
    let tacarr = (fst $ getTACFromBLExpr (AbsGrammar.ArrayEl con a ar l r) c) in
    let d = (snd $ getTACFromBLExpr (AbsGrammar.ArrayEl con a ar l r) c) in
    let arrtacs = (blexprContent tacarr) in
    let typ = getType con in
      case (blexprArray tacarr) of
        CArrInfo offset base _ ->
          let cont = arrtacs {text = ((TacArrayWrite base offset source (getTACType typ)):(tail (text arrtacs)))} in
          (tacarr {blexprContent = cont, blexprAddr = source}, d)


-- Given a LExpr and a counter, returns a couple (LExpr TACS, updated counter)
getTACFromLExpr:: LExpr TypeCheckRes -> Integer -> (LExpr TACS, Integer)
getTACFromLExpr lexpr c = case lexpr of

  AbsGrammar.BasLExpr _ _ blexpr ->
    let tacl = fst $ getTACFromBLExpr blexpr c in
    let d = snd $ getTACFromBLExpr blexpr c in
      
      (AbsGrammar.BasLExpr (blexprContent tacl) (blexprAddr tacl) tacl, d)

  AbsGrammar.IncDec tres _ op lexpr ->
    let valDest = newTemp c in
    let newT = newTemp $ c+1 in
    let tacl = fst $ getTACFromLExpr lexpr (c+2) in
    let d = snd $ getTACFromLExpr lexpr (c+2) in
    let ltacs = lexprContent tacl in
    let val = lexprAddr tacl in
    let typ = getTACType (SimpTyp T_Int) in
    let inc = CInt 1 in
    let tacop = (snd $ getIncDecInfo op) in
    let pre = (fst $ getIncDecInfo op) in
    let res = if pre then (TacAssignBinOp valDest tacop val inc typ) else (TacCopy valDest val typ) in
    let tacl2 = (fst $ getTACNoSidesFromLExpr lexpr d) in
    let e = (snd $ getTACNoSidesFromLExpr lexpr d) in
    let val2 = (lexprAddr tacl2) in
    let assign = (fst $ getAssignTACFromLExpr lexpr e newT) in
    let f = (snd $ getAssignTACFromLExpr lexpr e newT) in
    let update = ((text (lexprContent assign)) ++ [TacAssignBinOp newT tacop val2 inc typ]) in
      
      (AbsGrammar.IncDec (ltacs {text = (update ++ (res:((text (lexprContent tacl2)) ++ (text (lexprContent tacl)))))}) valDest op tacl, f)

  AbsGrammar.Deref ty _ rexp ->
    let newT = newTemp c in
    let tacr = (fst $ getValFromRExpr rexp (c+1)) in
    let d = (snd $ getValFromRExpr rexp (c+1)) in
    let rtacs = (rexprContent tacr) in
    
      (AbsGrammar.Deref (rtacs {text = ((TacAssignDeref newT (rexprAddr tacr) (getTACType $ getType ty)) : (text rtacs))}) newT tacr, d)


getIncDecInfo :: IncDecOp -> (Bool, TBinOp)
getIncDecInfo op = case op of
  AbsGrammar.PreInc -> (True, IntAdd)
  AbsGrammar.PreDecr -> (True, IntSub)
  AbsGrammar.PostInc -> (False, IntAdd)
  AbsGrammar.PostDecr -> (False, IntSub)


getTACNoSidesFromLExpr lexpr c = case lexpr of
  AbsGrammar.IncDec _ _ _ l -> getTACNoSidesFromLExpr l c
  _ -> getTACFromLExpr lexpr c


-- Given a BLExpr, a counter, the label begin and the label end, returns a couple (BLExpr TACS, updated counter)
getTACFromBLExpr:: BLExpr TypeCheckRes -> Integer -> (BLExpr TACS, Integer)
getTACFromBLExpr blexpr c = case blexpr of

  AbsGrammar.ParLExpr tres _ _ lexpr ->
    let tacl = (fst $ getTACFromLExpr lexpr c) in
    let d = (snd $ getTACFromLExpr lexpr c) in
      (AbsGrammar.ParLExpr (lexprContent tacl) (lexprAddr tacl) (CAddr "") tacl, d)

  AbsGrammar.Id ty _ _ str ->
    let env = getEnv ty in
    let entry = head $ findWithDefault [] str env in
    let pos = (varPos entry) in
    let mod = (varMod entry) in
      case mod of
        Modality_ref -> (AbsGrammar.Id (TACS [TacAssignDeref (newTemp c) (getIdentifier pos str) (getTACType (getType ty))] [] []) (newTemp c) (CAddr "") str, c + 1)
        Modality_valres -> (AbsGrammar.Id (TACS [] [] []) (CAddr ((show (getIdentifier pos str)) ++ "$valres")) (CAddr "") str, c)
        _ -> (AbsGrammar.Id (TACS [] [] []) (getIdentifier pos str) (CAddr "") str, c)

  AbsGrammar.ArrayEl tres _ _ (ArrayEl con ad ar lexp rexp) r ->
    let arrSize = (getNum (getType con)) in
    let arr = (ArrayEl con ad ar lexp rexp) in
    let offset = newTemp c in
    let newT = newTemp (c+1) in
    let newOut = newLab (c+2) in
    let newIn = newLab (c+3) in
    let tacr = (fst $ getValFromRExpr r (c+4)) in
    let d = (snd $ getValFromRExpr r (c+4)) in
    let rtacs = (rexprContent tacr) in
    let tacl = (fst (getTACFromBLExpr arr d)) in
    let e = (snd (getTACFromBLExpr arr d)) in
    let typ = (getType tres) in
    let texp = (getType (rexprContent r)) in
      case (blexprArray tacl) of
        CArrInfo oldOffset base check ->
          let checkBounds = if check
                              then
                                [TacLabel newIn, TacError ("array index out of bounds at " ++ (show (getPos tres))), TacLabel newOut, TacRelConJump newIn True LtInt (rexprAddr tacr) (CInt arrSize), TacRelConJump newOut True LtInt (rexprAddr tacr) (CInt 0)]
                              else
                                [] in
            (AbsGrammar.ArrayEl (rtacs {text = ((TacArrayRead newT base offset (getTACType typ)):(TacAssignBinOp offset IntAdd offset oldOffset (getTACType (SimpTyp T_Int))):(TacAssignBinOp offset IntMul (rexprAddr tacr) (CInt (getSize tres)) (getTACType (SimpTyp T_Int))):checkBounds ++ (text rtacs) ++ (tail (text (blexprContent tacl)))), dataMem = ((dataMem rtacs) ++ (dataMem (blexprContent tacl)))}) newT (CArrInfo offset base check) tacl tacr, e)


  AbsGrammar.ArrayEl tres _ _ lexpr r ->
    let arrSize = (getNum (getType (blexprContent lexpr))) in
    let idStr = (getId (blexprContent lexpr)) in
    let entry = head $ findWithDefault [] idStr (getEnv tres) in
    let check = (varCheck entry) in
    let offset = newTemp c in
    let newT = newTemp (c+1) in
    let newOut = newLab (c+2) in
    let newIn = newLab (c+3) in
    let tacr = (fst $ getValFromRExpr r (c+4)) in
    let d = (snd $ getValFromRExpr r (c+4)) in
    let rtacs = (rexprContent tacr) in
    let tacl = (fst $ getTACFromBLExpr lexpr d) in
    let e = (snd $ getTACFromBLExpr lexpr d) in
    let env = (getEnv tres) in
    let typ = (getType tres) in
    let base = (blexprAddr tacl) in
    let texp = (getType (rexprContent r)) in
    let checkBounds = if check
                              then
                                [TacLabel newIn, TacError ("array index out of bounds at " ++ (show (getPos tres))), TacLabel newOut, TacRelConJump newIn True LtInt (rexprAddr tacr) (CInt arrSize), TacRelConJump newOut True LtInt (rexprAddr tacr) (CInt 0)]
                              else
                                [] in
      (AbsGrammar.ArrayEl (rtacs {text = ((TacArrayRead newT base offset (getTACType typ)):(TacAssignBinOp offset IntMul (rexprAddr tacr) (CInt (getSize tres)) (getTACType (SimpTyp T_Int))):(checkBounds ++ (text rtacs) ++ (text (blexprContent tacl)))), dataMem = ((dataMem rtacs) ++ (dataMem (blexprContent tacl)))}) newT (CArrInfo offset base check) tacl tacr, e)



-- Given a JumpStmt, a counter, the label begin and the label end, returns a couple (JumpStmt TACS, updated counter)
getTACFromJumpStmt:: JumpStmt TypeCheckRes -> Integer -> Lab -> Lab -> (JumpStmt TACS, Integer)
getTACFromJumpStmt jump c beg end = case jump of
  
  AbsGrammar.Break _ -> (AbsGrammar.Break (TACS [TacJump end] [] []), c)
  AbsGrammar.Continue _ -> (AbsGrammar.Continue (TACS [TacJump beg] [] []), c)
  AbsGrammar.RetExpVoid _ -> (AbsGrammar.RetExpVoid (TACS [TacRetVoid] [] []), c)
  
  AbsGrammar.RetExp tres rexpr ->
    let tacr = fst $ getValFromRExpr rexpr c in
    let d = snd $ getValFromRExpr rexpr c in
    let rtacs = (rexprContent tacr) in
    let env = getEnv tres in
    let tre = varType . head $ findWithDefault [] "return" env in
    let texp = getType $ rexprContent rexpr in
      
      if tre == texp
        then (AbsGrammar.RetExp (rtacs {text = ((TacRet (rexprAddr tacr) (getTACType tre)):(text rtacs))}) tacr, d)
        else let newT = newTemp d in
            
            (AbsGrammar.RetExp (rtacs {text = ((TacRet newT (getTACType tre)):(cast newT (rexprAddr tacr) tre texp):(text rtacs))}) tacr, d + 1)




-- Given a Posn for a identifier and the string of that identifier returns an addr with the information about the location of declaration
getIdentifier:: Posn -> String -> Addr
getIdentifier p s = CAddr $ s ++ "@" ++ (show $ getPosRow p) ++ "," ++ (show $ getPosColumn p)


-- Given a counter creates a new temp using that counter
newTemp :: Integer -> Addr
newTemp c = CAddr $ "t" ++ (show c)


-- Given a counter creates a new label using that counter
newLab :: Integer -> String
newLab c = "L" ++ (show c)


newStr :: Integer -> Addr
newStr c = CAddr ("str" ++ (show c))

-- Given a type returns a string with that type
getTACType :: Type -> String
getTACType t = case t of
  SimpTyp (T_String) -> "addr"
  SimpTyp s -> show s
  Point _ _ -> "addr"
  Type.Array _ _ _ -> "addr" -- array ref args


getEqOperatorType::Type -> EqOp -> TRelOp
getEqOperatorType ty op = case op of
  AbsGrammar.Eq -> case ty of
    SimpTyp T_Int -> EqInt
    Point _ _ -> EqLong
    SimpTyp T_String -> EqLong
    SimpTyp T_Char -> EqChar
    SimpTyp T_Float64 -> EqFloat
    SimpTyp T_Boolean -> EqBoolean
  AbsGrammar.Neq -> case ty of
    SimpTyp T_Int -> NeqInt
    Point _ _ -> NeqLong
    SimpTyp T_String -> NeqLong
    SimpTyp T_Char -> NeqChar
    SimpTyp T_Float64 -> NeqFloat
    SimpTyp T_Boolean -> NeqBoolean  


getCoOperatorType::Type -> CoOp -> TRelOp
getCoOperatorType ty op = case op of    
  AbsGrammar.Lt -> case ty of
    SimpTyp T_Int -> LtInt
    Point _ _ -> LtLong
    SimpTyp T_String -> LtLong
    SimpTyp T_Char -> LtChar
    SimpTyp T_Float64 -> LtFloat
    SimpTyp T_Boolean -> LtBoolean  
  AbsGrammar.LtE -> case ty of
    SimpTyp T_Int -> LtEInt
    Point _ _ -> LtELong
    SimpTyp T_String -> LtELong
    SimpTyp T_Char -> LtEChar
    SimpTyp T_Float64 -> LtEFloat
    SimpTyp T_Boolean -> LtEBoolean  
  AbsGrammar.Gt -> case ty of
    SimpTyp T_Int -> GtInt
    Point _ _ -> GtLong
    SimpTyp T_String -> GtLong
    SimpTyp T_Char -> GtChar
    SimpTyp T_Float64 -> GtFloat
    SimpTyp T_Boolean -> GtBoolean  
  AbsGrammar.GtE -> case ty of
    SimpTyp T_Int -> GtEInt
    Point _ _ -> GtELong
    SimpTyp T_String -> GtELong
    SimpTyp T_Char -> GtELong
    SimpTyp T_Float64 -> GtELong
    SimpTyp T_Boolean -> GtEBoolean  


-- Given a type and an operator return a operator for that type
getOperator:: Type -> Op -> TBinOp
getOperator ty op = case op of 
  Add -> case ty of
    SimpTyp T_Int -> IntAdd
    SimpTyp T_Float64 -> FloatAdd
  Sub -> case ty of
    SimpTyp T_Int -> IntSub
    SimpTyp T_Float64 -> FloatSub
  Mul -> case ty of
    SimpTyp T_Int -> IntMul
    SimpTyp T_Float64 -> FloatMul
  Div -> case ty of
    SimpTyp T_Int -> IntDiv
    SimpTyp T_Float64 -> FloatDiv
  Pow -> case ty of
    SimpTyp T_Int -> IntPow
    SimpTyp T_Float64 -> FloatPow
  AbsGrammar.Mod -> TACGen.Mod


-- Given: the addr where to put the cast, the addr of what to cast, the result type and the start type returns a TAC with the casting
cast:: Addr -> Addr -> Type -> Type -> TAC
cast ad1 ad2 ty1 ty2 = case (ty1, ty2) of 
  (SimpTyp T_Float64, SimpTyp T_Int) -> TacAssignUnOp ad1 IntToFloat ad2 (getTACType (SimpTyp T_Float64))


-- Given two types returns the most general of the two
superType:: Type -> Type -> Type
superType (SimpTyp T_Int) (SimpTyp T_Float64) = SimpTyp T_Float64
superType (SimpTyp T_Float64) (SimpTyp T_Int) = SimpTyp T_Float64
superType (Point ty1 siz1) (Point ty2 siz2) = Point (superType ty1 ty2) (if siz1 > siz2 then siz1 else siz2)
superType (Type.Array ty1 num1 siz1) (Type.Array ty2 num2 siz2) = Type.Array (superType ty1 ty2) num1 (if siz1 > siz2 then siz1 else siz2)
superType x y = x 
