module Wasm.Wasm where
  data Module = Module {
                  modName          :: String,
                  modFuncTypeDescs :: [TypeDesc FunctionType],
                  modHeapTypeDescs :: [TypeDesc HeapType],
                  modImports       :: [FunctionImport],
                  modGlobals       :: [Global],
                  modFunctions     :: [Function]
                } deriving Show

  data Function = Function {
                    funcName   :: String,
                    funcExport :: Bool,
                    funcType   :: FunctionType,
                    funcBody   :: Expression
                  } deriving (Show, Eq)
  
  data Global = Global {
                  globalName    :: String,
                  globalExport  :: Bool,
                  globalType    :: ValType,
                  globalExpr    :: Expression
                } deriving (Show, Eq)

  data FunctionImport = {-GlobalImport {
                             globImpModName  :: String,
                             globImpSrcName  :: String,
                             globImpDestName :: String,
                             globImpType     :: ValType
                           }
                         | -}FunctionImport {
                             funcImpMod     :: String,
                             funcImpSrc     :: String,
                             funcImpDest    :: String,
                             funcImpTypeUse :: FunctionType
                           }  deriving (Show, Eq)

  class Descriptive a
  instance Descriptive FunctionType
  instance Descriptive HeapType

  data TypeDesc a = TypeDesc { 
            typeDescId   :: String,
            typeDescType :: a
        }
      -- | RecTypeDesc [TypeDesc]
          deriving (Show, Eq)


  
  data ValType = NumType NumberType
               | RefType ReferenceType
                 deriving (Show, Eq)

  data NumberType = I32 | I64 | F32 | F64 deriving (Show, Eq)
  
  data HeapType = Eq
                | Array Bool ValType --mutable or not
                | Struct [(Bool, ValType)]
                | I31
                | Func String
                | HeapTypeDesc TypeIdx
                  deriving (Show, Eq)
  

  data FunctionType = FunctionType {
                        funcParams :: [ValType],
                        funcResult :: [ValType] -- we don't use multiple, but more result values are supported by wasm
                      }
                    | TypeUse TypeIdx
                        deriving (Show, Eq)
  
  type TypeIdx = String

{-
  data TypeUse = TypeUseRef String
               | TypeUseNew FunctionType
                 deriving (Show, Eq)
-}
  data ReferenceType = Ref HeapType
                     | NullableRef HeapType
                     deriving (Show, Eq)

{-
  typeUseHeap :: TypeDesc -> HeapType
  typeUseHeap (TypeDesc _ (Ref t)) = t

  typeUseFunc :: TypeDesc -> FunctionType
  typeUseFunc (TypeDesc _ (FuncRef t)) = t
-}

  type Expression = [Instruction]

  data Instruction = Const_int32 Integer
                   | Const_int64 Integer
                   | Const_float32 Double
                   | Const_float64 Double
               --    | RefFunc String
                   | I31_new
                   | I31_get_u
                   | I31_get_s
                   | Add_int
                   | Mul_int
                   | Add_float
                   | Mul_float
                   | Call String
                   | CallRef
                   | Ref_func String
                   | Ref_cast ReferenceType
                   | Ref_null ReferenceType
                   | Struct_new TypeIdx
                   | Struct_get TypeIdx Int
                   | Array_new TypeIdx
                   | Array_new_fixed TypeIdx Int
                   | Array_get TypeIdx
                   | Local_get Int
                   | Global_get String
                   | IfThenElse Expression Expression
                -- | GetClosRef Int       -- Temps need better implementation
                -- | TempGetImportRef String
                -- | TempMakeClos
                   | Missed String                     -- meant for not implemented expressions
                    deriving (Show, Eq)