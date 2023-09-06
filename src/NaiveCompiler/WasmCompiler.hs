module NaiveCompiler.WasmCompiler where

  import qualified NaiveCompiler.IntermediateLambda as IL
  import qualified NaiveCompiler.Runtime as RT
  import Common.TypeClass
  import Common.CompileOptions
  import Wasm.Wasm
  import Data.List
  import Data.Either

  data Environment = Environment {
                       globalIds            :: [String],
                       funcParamClosIds     :: [String],
                       funcParamIds         :: [String],
                       funcClosIds          :: [String],
                       funcNoParamNoClosIds :: [String],
                       exportIds            :: [String],
                       foreignFuncs         :: [String] -- (fileName, identifier)
                    } deriving Show


  createWasmModule :: CompileOptions -> [IL.Module] -> Module
  createWasmModule opts ilMods = Module
                              "main"
                              RT.functionTypes
                              RT.heapTypes
                              (RT.functionImports ++ imports)
                              (lefts (map (convertILBind opts env) globals))
                              (rights (concat [map (convertILBind opts env) funcParamCloss
                                              ,map (convertILBind opts env) funcParams
                                              ,map (convertILBind opts env) funcCloss
                                              ,map (convertILBind opts env) funcNoParamNoCloss]))

      where
        env = Environment
                (map IL.identifier globals)
                (map IL.identifier funcParamCloss)
                (map IL.identifier funcParams)
                (map IL.identifier funcCloss)
                (map IL.identifier funcNoParamNoCloss)
                exportIds
                (map nameSpacedForeignID getForeigns)

        allDecls             = concatMap IL.declarations ilMods
        globals             = filter (((==) IL.LiteralConst) . IL.kind) allDecls
        funcParamCloss      = filter (((==) IL.FuncWithParamAndClosArgs) . IL.kind) allDecls
        funcParams          = filter (((==) IL.FuncWithParam) . IL.kind) allDecls
        funcCloss           = filter (((==) IL.FuncWithClosArgs) . IL.kind) allDecls
        funcNoParamNoCloss  = filter (((==) IL.FuncWithNoParamNoClosArgs) . IL.kind) allDecls
        {-(lits, evalOrLambda) = partition isGlobal allDecls
        funcIds              = map IL.identifier evalOrLambda
        globalIds            = map IL.identifier lits
        funcs                = concatMap snd convertedBinds
        globals              = concatMap fst convertedBinds-}
        convertedBinds       = map (convertILBind opts env) allDecls
                                
        exportIds            = concatMap IL.exports ilMods
        getForeigns          = concatMap IL.foreigns ilMods--(\x -> map (\y -> (IL.moduleName x, y)) (IL.foreigns x)) ilMods
        imports              = map (\(mod, id) -> FunctionImport mod id (nameSpacedForeignID (mod, id)) (TypeUse RT.typeIdFuncParamClos{- officialy is RT.typeIdFuncParam-})) getForeigns
        --getForeigns      = concat $ map (\x -> (IL.moduleName x ++ "." ++ IL.foreigns x)) ilMods 
        {-isGlobal (IL.Bind _ _ IL.LiteralConst) = True
        isGlobal _                         = False
-}
  nameSpacedForeignID :: (String, String) -> String
  nameSpacedForeignID (x, y) = intercalate "." [x, y]

  convertILBind :: CompileOptions -> Environment -> IL.Bind -> Either Global Function
  convertILBind opts env (IL.Bind id expr IL.LiteralConst)
    = Left $ Global id (id `elem` (exportIds env)) (RefType (Ref Eq)) (convertILExpr opts env IL.LiteralConst expr)
  convertILBind opts env (IL.Bind id expr kind)
    = Right $ Function
                id
                (id `elem` (exportIds env))
                (getTypeUseForBindKind kind)
                (convertILExpr opts env kind expr)
  

  -- afspraak: alle functies krijgen closureargs mee
  getTypeUseForBindKind :: IL.BindKind -> FunctionType
  getTypeUseForBindKind kind = case kind of
      IL.FuncWithParamAndClosArgs     -> TypeUse RT.typeIdFuncParamClos
      IL.FuncWithParam                -> TypeUse RT.typeIdFuncParamClos --RT.typeIdFuncParam
      IL.FuncWithClosArgs             -> TypeUse RT.typeIdFuncClos
      IL.FuncWithNoParamNoClosArgs    -> TypeUse RT.typeIdFuncClos --RT.typeIdFuncNoParamNoClos



  convertILExpr :: CompileOptions -> Environment -> IL.BindKind -> IL.Expression -> Expression
  convertILExpr opts env kind expr@(IL.IntegerLiteral x)    = getCorrectConstant opts expr --[Const_int64 x, Struct_new RT.typeIdBoxedi64]
  convertILExpr opts env kind expr@(IL.NumberLiteral x)     = getCorrectConstant opts expr --[Const_float64 x, Struct_new RT.typeIdBoxedf64]
  convertILExpr opts env kind (IL.BooleanLiteral x)    = [Const_int32 (if x then 1 else 0), I31_new]
  convertILExpr opts env kind (IL.ArrayLiteral x)      = (concat $ map (convertILExpr opts env kind) x)
                                                ++ [Array_new_fixed RT.typeIdClassDict (length x)]
  convertILExpr opts env kind (IL.GetTypeClassElem n expr) = (convertILExpr opts env kind expr)
                                                     ++ [Ref_cast (Ref (HeapTypeDesc RT.typeIdClassDict)),
                                                         Const_int32 (toInteger n),
                                                         Array_get RT.typeIdClassDict]
  convertILExpr opts env kind (IL.Get id)
        | id `elem` (globalIds env)          = [Call id] --[Global_get id]
        | (id `elem` ((funcParamClosIds env) ++ (funcParamIds env)))
          = [Ref_func id] ++ passedClosureInstr  ++ [Call RT.funcIdNewClosure] -- ready for apply
        | (id `elem` ((funcClosIds env) ++ (funcNoParamNoClosIds env)))
          = passedClosureInstr ++ [Call id]   -- just call it, it doesn't need an arg, so no apply
        | id `elem` (foreignFuncs env)       = [Ref_func id,
                                                Array_new_fixed RT.typeIdClosureArgArray 0,
                                                Call RT.funcIdNewClosure]
        | otherwise                          = [Missed $ "id " ++ id ++ " not found. Foreigns are " ++ (intercalate " " (foreignFuncs env))]
    where passedClosureInstr = if (kind `elem` [IL.FuncWithParamAndClosArgs, IL.FuncWithParam])
                                then [Local_get 0, Local_get 1, Call RT.funcIdAddToArgArray]
                                else if (kind == IL.LiteralConst)
                                     then [Array_new_fixed RT.typeIdClosureArgArray 0]
                                     else [Local_get 0]
  convertILExpr opts env kind (IL.GetLocal n)          = [Local_get n]
  convertILExpr opts env kind (IL.GetClosureVar n)     = [Local_get 0,
                                                Const_int32 (toInteger n),
                                                Call RT.funcIdGetSingleClosureArg]
  convertILExpr opts env kind (IL.Apply e1 e2)         = (convertILExpr opts env kind e1)
                                                ++ [Ref_cast (Ref (HeapTypeDesc RT.typeIdClosureTop))]
                                               -- ++ [Local_get 0, Local_get 1,   -- TODO! wat als param null is?
                                               --     Call RT.funcIdAddToArgArray, Call RT.funcIdNewClosure]
                                                ++ (convertILExpr opts env kind e2)
                                                ++ [Call RT.funcIdApply]
  convertILExpr opts env kind (IL.Lambda _ _ expr)     = convertILExpr opts env kind expr
  convertILExpr opts env kind (IL.SingleBoolCase testExpr ifTrueExpr ifFalseExpr)
    = (convertILExpr opts env kind testExpr)
      ++ [Ref_cast (Ref I31), I31_get_s]
      ++ [IfThenElse (convertILExpr opts env kind ifTrueExpr) (convertILExpr opts env kind ifFalseExpr)]
  convertILExpr opts env _ x = [Missed $ (show x) ++ "foreigns: " ++ (concat (foreignFuncs env))]
  
  
  getCorrectConstant :: CompileOptions -> IL.Expression -> Expression
  getCorrectConstant (CompileOptions Integer31 _) (IL.IntegerLiteral x)
    = [Const_int32 x, I31_new]
  getCorrectConstant (CompileOptions Integer32 _) (IL.IntegerLiteral x)
    = [Const_int32 x, Struct_new RT.typeIdBoxedi32]
  getCorrectConstant (CompileOptions Integer64 _) (IL.IntegerLiteral x)
    = [Const_int64 x, Struct_new RT.typeIdBoxedi64]
  getCorrectConstant (CompileOptions _ Float32) (IL.NumberLiteral x)
    = [Const_float32 x, Struct_new RT.typeIdBoxedf32]
  getCorrectConstant (CompileOptions _ Float64) (IL.NumberLiteral x)
    = [Const_float64 x, Struct_new RT.typeIdBoxedf64]

