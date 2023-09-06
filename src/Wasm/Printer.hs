module Wasm.Printer where

  import Data.List
  import qualified Data.Text as T
  import Wasm.Wasm

  space = " "

  -- create a string from a module
  printModule :: Module -> String
  printModule m = indent
                   ("(module\n"
                    ++ (intercalate "\n" (map printHeapTypeDescription (modHeapTypeDescs m))) ++ "\n\n"
                    ++ (intercalate "\n" (map printFunctionTypeDescription (modFuncTypeDescs m))) ++ "\n\n"
                    ++ (intercalate "\n" (map printImport (modImports m))) ++ "\n\n"
                    ++ (intercalate "\n" (map printGlobal (modGlobals m))) ++ "\n\n"
                    -- TODO! moeten foreigns ook bij declares?
                    ++ (intercalate "\n" (map printDeclare (map funcName (modFunctions m)))) ++ "\n\n"
                    ++ (intercalate "\n" (map printDeclare (map funcImpDest (modImports m)))) ++ "\n\n"
                    ++ (intercalate "\n\n" (map printFunction (modFunctions m)))
                    ++ "\n)"
                   )
                   1

  printDeclare :: String -> String
  printDeclare fName = "(elem declare func $" ++ fName ++ ")"

  printImport :: FunctionImport -> String
  printImport (FunctionImport modName srcId destId tUse)
    = "(import \"" ++ modName ++ "\" \"" ++ srcId ++ "\" (func $" ++ destId ++ " " ++ printFunctionType tUse ++ "))"

  printGlobal :: Global -> String
  printGlobal (Global name export gtype expression)
    = "(func $"           -- -> was global
      ++ name ++ " "
      ++ (if export then "(export \"" ++ name ++ "\") " else "")
      ++ "(result "
      ++ printValType gtype ++ ")\n"
      ++ printExpression expression
      ++ "\n)" 

  printFunction :: Function -> String
  printFunction (Function fName fExp fType fBody)
    = "(func $" ++ fName ++ " "
      ++ (if fExp then "(export \"" ++ fName ++ "\") " else "")
      ++ printFunctionType fType ++ "\n"
      ++ printExpression fBody
      ++ "  \n)"


  printHeapTypeDescription :: TypeDesc HeapType -> String
  printHeapTypeDescription (TypeDesc id ht) = "(type $" ++ id ++ " (" ++ printHeapType ht ++ "))"

  printFunctionTypeDescription :: TypeDesc FunctionType -> String
  printFunctionTypeDescription (TypeDesc id ft) = "(type $" ++ id ++ " (func " ++ printFunctionType ft ++"))"

  printValType :: ValType -> String
  printValType (NumType t) = printNumType t
  printValType (RefType t) = printReferenceType t

  printNumType :: NumberType -> String
  printNumType I32 = "i32"
  printNumType I64 = "i64"
  printNumType F32 = "f32"
  printNumType F64 = "f64"

  printReferenceType :: ReferenceType -> String
  printReferenceType (Ref t)     = "(ref " ++ printHeapType t ++ ")"
  printReferenceType (NullableRef t) = "(ref null " ++ printHeapType t ++ ")"

  printHeapType :: HeapType -> String
  printHeapType Eq                = "eq"
  printHeapType I31               = "i31"
  printHeapType (Array mut t)     = "array (" ++ (if mut then "mut " else "") ++ printValType t ++ ")"
  printHeapType (Struct fields)   = "struct " ++ (concat $ map printField fields)
      where printField (mut, t)   = "(field " ++ (if mut then "mut " else "") ++ printValType t ++")"
  printHeapType (Func s)          = if (s /= "")
                                    then "(func $"  ++ s ++ ")"
                                    else "func"
  printHeapType (HeapTypeDesc s)  = "$" ++ s


  printFunctionType :: FunctionType -> String
  printFunctionType (TypeUse s)      = "(type $" ++ s ++ ")"
  printFunctionType (FunctionType par res) = (concatMap paramString par)
                                           ++ (concatMap resultString res)
        where paramString  x = "(param " ++ printValType x ++ ")"
              resultString x = "(result " ++ printValType x ++ ")"


  -- create string of Wasm Expression
  printExpression :: Expression -> String
  printExpression = intercalate "\n" . map printInstruction

  printInstruction t = case t of
    (Const_int32 x)         -> "i32.const " ++ show x
    (Const_int64 x)         -> "i64.const " ++ show x
    (Const_float32 x)       -> "f32.const " ++ show x
    (Const_float64 x)       -> "f64.const " ++ show x
    (I31_new)               -> "i31.new"
    (I31_get_u)             -> "i31.get_u"
    (I31_get_s)             -> "i31.get_s"
    (Call s)                -> "call $" ++ s
    (Ref_func s)            -> "ref.func $" ++ s
    (Ref_cast t)            -> "ref.cast " ++ printReferenceType t
    (Ref_null t)            -> "ref.null " ++ printReferenceType t
    (Struct_new s)          -> "struct.new $" ++ s
    (Struct_get s n)        -> "struct.get $" ++ s ++ " " ++ show n
    (Array_new s)           -> "array.new $" ++ s
    (Array_new_fixed s n)   -> "array.new_fixed $" ++ s ++ " " ++ (show n)
    (Array_get s)           -> "array.get $" ++ s
    (Local_get n)           -> "local.get " ++ (show n)
    (Global_get s)          -> "global.get $" ++ s    
    (IfThenElse e1 e2)      -> "(if (result " ++ printReferenceType (NullableRef Eq) ++ ")\n(then\n"
                               ++ printExpression e1
                               ++ "\n)\n(else\n"
                               ++ printExpression e2 
                               ++ "\n)\n)"

    (Missed s)              -> "!!!! MISSED !!! ->" ++ s


  -- create indentation with spaces
  indent :: String -> Int -> String
  indent str startLevel = prune $ go str startLevel where
    go [] _ = []
    go (x:xs) level
        | x == '('  = [x] ++ (go xs (level+1))
        | x == ')'  = [x] ++ (go  xs (level-1))
        | x == '\n' = [x] ++ replicate (2*level) ' ' ++ (go (dropWhile (==' ') xs) level)
        | otherwise = [x] ++ (go xs level)

    -- remove unnecessary indentation on lines with ')'
    prune [] = []
    prune (' ':' ':')':xs) = ")" ++ prune (dropWhile (== ' ') xs)
    prune (x:xs) = [x] ++ prune xs 

  printInitScript :: Module -> String
  printInitScript m = (concatMap (\x -> (printInput x) ++ "\n" ++ (printRegister x) ++ "\n") moduleNames)
                       ++ printInput (modName m)
    where
      moduleNames = nub $ "runtime" : (map funcImpMod (modImports m))

  printInput :: String -> String
  printInput name = "(input $" ++ name ++ " \"" ++ name ++ ".wat\")"

  printRegister :: String -> String
  printRegister name = "(register \"" ++ name ++ "\" $" ++ name ++ ")"