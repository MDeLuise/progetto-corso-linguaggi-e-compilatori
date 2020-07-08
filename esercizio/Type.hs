-- Module needed because types are imported from both AbsGrammar and TypeChecking, and TypeChecking also imports AbsGrammar.
-- Thus this is needed to avoid import cycles


module Type where

data SimpleType
    = T_Boolean
    | T_Char
    | T_Float64
    | T_Int
    | T_Void
    | T_String
  deriving (Eq, Ord, Read)


instance Show SimpleType where
  show ty = case ty of
    T_Boolean -> "boolean"
    T_Char -> "char"
    T_Float64 -> "float64"
    T_Int -> "int"
    T_Void -> "void"
    T_String -> "string"


data Type
  = SimpTyp {getSimpleType:: SimpleType}
  | Array {typeGetType:: Type, getNum:: Integer, getSizeElem :: Integer}
  | Point {typeGetType:: Type, getSizePointee :: Integer}
  | Bottom
  deriving (Eq, Ord, Read)


instance Show Type where
  show ty = case ty of
    SimpTyp sTy -> show sTy
    Array aTy aDim _ -> "array[" ++ show aTy ++ "]"
    Point pty _ -> "pointer[" ++ show pty ++ "]"
    Bottom -> "bottom"
