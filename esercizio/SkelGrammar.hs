module SkelGrammar where

-- Haskell module generated by the BNF converter

import AbsGrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Show attr => (Ident attr) -> Result
transIdent x = case x of
  Ident _ string -> failure x
transBoolean :: Show attr => (Boolean attr) -> Result
transBoolean x = case x of
  Boolean_true _ -> failure x
  Boolean_false _ -> failure x
transBasicType :: Show attr => (BasicType attr) -> Result
transBasicType x = case x of
  BasicType_boolean _ -> failure x
  BasicType_char _ -> failure x
  BasicType_float64 _ -> failure x
  BasicType_int _ -> failure x
  BasicType_void _ -> failure x
  BasicType_String _ -> failure x
transModality :: Modality -> Result
transModality x = case x of
  Modality1 -> failure x
  Modality_val -> failure x
  Modality_ref -> failure x
  Modality_valres -> failure x
  Modality_const -> failure x
transNewLine :: NewLine -> Result
transNewLine x = case x of
  NewLine1 -> failure x
transProgram :: Show attr => (Program attr) -> Result
transProgram x = case x of
  Prog _ blockdecl -> failure x
transRExpr :: Show attr => (RExpr attr) -> Result
transRExpr x = case x of
  BoolBinOp _ _ Or rexpr1 rexpr2 -> failure x
  BoolBinOp _ _ And rexpr1 rexpr2 -> failure x
  Not _ _ rexpr -> failure x
  Equality _ _ Eq rexpr1 rexpr2 -> failure x
  Equality _ _ Neq rexpr1 rexpr2 -> failure x
  Comparison _ _ Lt rexpr1 rexpr2 -> failure x
  Comparison _ _ LtE rexpr1 rexpr2 -> failure x
  Comparison _ _ Gt rexpr1 rexpr2 -> failure x
  Comparison _ _ GtE rexpr1 rexpr2 -> failure x
  BinOp _ _ Add rexpr1 rexpr2 -> failure x
  BinOp _ _ Sub rexpr1 rexpr2 -> failure x
  BinOp _ _ Mul rexpr1 rexpr2 -> failure x
  BinOp _ _ Div rexpr1 rexpr2 -> failure x
  BinOp _ _ Mod rexpr1 rexpr2 -> failure x
  BinOp _ _ Pow rexpr1 rexpr2 -> failure x
  Neg _ _ rexpr -> failure x
  Ref _ _ lexpr -> failure x
  FCall _ _ ident rexprs -> failure x
  Int _ _ integer -> failure x
  Char _ _ char -> failure x
  String _ _ string -> failure x
  Float _ _ double -> failure x
  Bool _ _ boolean -> failure x
  Lexpr _ _ lexpr -> failure x
  IfRe _ _ rexpr1 rexpr2 rexpr3 -> failure x
transLExpr :: Show attr => (LExpr attr) -> Result
transLExpr x = case x of
  Deref _ _ rexpr -> failure x
  IncDec _ _ PreInc lexpr -> failure x
  IncDec _ _ PreDecr lexpr -> failure x
  IncDec _ _ PostInc lexpr -> failure x
  IncDec _ _ PostDecr lexpr -> failure x
  BasLExpr _ _ blexpr -> failure x
transBLExpr :: Show attr => (BLExpr attr) -> Result
transBLExpr x = case x of
  ArrayEl _ _ _ blexpr rexpr -> failure x
  Id _ _ _ str -> failure x
transTypeSpec :: Show attr => (TypeSpec attr) -> Result
transTypeSpec x = case x of
  BasTyp _ basictype -> failure x
  ArrDef _ typespec integer -> failure x
  ArrUnDef _ typespec -> failure x
  Pointer _ typespec -> failure x
transVarDeclInit :: Show attr => (VarDeclInit attr) -> Result
transVarDeclInit x = case x of
  VarDeclIn _ _ ident typespec complexrexpr check -> failure x
  ConDeclIn _ _ ident typespec complexrexpr check -> failure x
transComplexRExpr :: Show attr => (ComplexRExpr attr) -> Result
transComplexRExpr x = case x of
  Simple _ _ rexpr -> failure x
  Array _ _ complexrexprs -> failure x
transParameter :: Show attr => (Parameter attr) -> Result
transParameter x = case x of
  Param _ _ modality ident typespec -> failure x
transStmt :: Show attr => (Stmt attr) -> Result
transStmt x = case x of
  Comp _ blockdecl -> failure x
  ProcCall _ ident rexprs -> failure x
  Jmp _  jumpstmt -> failure x
  While _ rexpr blockdecl -> failure x
  DoWhile _ rexpr blockdecl -> failure x
  For _ ident rexpr1 rexpr2 blockdecl -> failure x
  Sel _ selectionstmt -> failure x
  Assgn _ lexpr assignmentop rexpr -> failure x
  LExprStmt _ lexpr -> failure x
  VarDec _ vardeclinit -> failure x
  FunDec _ ident parameters typespec blockdecl -> failure x
  Switch _ rexpr switchblock -> failure x
transSwitchBlock :: Show attr => (SwitchBlock attr) -> Result
transSwitchBlock x = case x of
  BlockSwitch _ switchmatchs -> failure x
transSwitchMatch :: Show attr => (SwitchMatch attr) -> Result
transSwitchMatch x = case x of
  Match _ rexpr blockdecl -> failure x
  Default _ blockdecl -> failure x
transBlockDecl :: Show attr => (BlockDecl attr) -> Result
transBlockDecl x = case x of
  Block _ stmts -> failure x
transAssignment_op :: Show a => Assignment_op a -> Result
transAssignment_op x = case x of
  Assign _ -> failure x
  AssgnMul _ -> failure x
  AssgnAdd _ -> failure x
  AssgnDiv _ -> failure x
  AssgnSub _ -> failure x
  AssgnPow _ -> failure x
  AssgnAnd _ -> failure x
  AssgnOr _ -> failure x
transJumpStmt :: Show attr => (JumpStmt attr) -> Result
transJumpStmt x = case x of
  Break _ -> failure x
  Continue _ -> failure x
  RetExpVoid _ -> failure x
  RetExp _ rexpr -> failure x
transSelectionStmt :: Show attr => (SelectionStmt attr) -> Result
transSelectionStmt x = case x of
  IfNoElse _ rexpr blockdecl -> failure x
  IfElse _ rexpr blockdecl1 blockdecl2 -> failure x

