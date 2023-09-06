module Common.TypeClass where
  import Common.PureScriptUtility

  import qualified Language.PureScript.CoreFn as CF
  import qualified Language.PureScript.AST.Literals as PSLit
  import qualified Language.PureScript.Names as PS
  import qualified Language.PureScript.PSString as PS
  import Language.PureScript.AST.SourcePos (nullSourceSpan)

  import Data.List
  import Data.Text (pack)

  data TypeClass = TypeClass {
                    typeclassname          :: String,
                    typeclassfunctions     :: [String]
                   } deriving (Show, Eq) -- name of typeclass, [name of functions]

  data TypeClassInstance = Instance {
                            instanceName        :: String,
                            instanceTypeclass   :: TypeClass,
                            instanceExpressions :: [CF.Expr CF.Ann]
                           } deriving (Show, Eq)

  type InstanceBind = (String, CF.Expr CF.Ann)

  findTypeClasses :: [CF.Bind CF.Ann] -> [TypeClass]
  findTypeClasses binds = map (\x -> TypeClass (getIdentifier x)
                                               (findTypeClassFunctions binds (getIdentifier x))
                              ) typeclassConstructorBinds
            where 
              typeclassConstructorBinds = filter isTypeClassConstructor binds
              getIdentifier (CF.NonRec _ id _) = identToString id
              getIdentifier _                  = "" -- #TODO to be implemented
              validTypeClassName = takeWhile (/= '$')
              --constructorsAndInstanceFuncs = map (\x -> (getIdentifier x, (findTypeClassInstances binds) $ getIdentifier x)) typeclassConstructorBinds

  
  -- #TODO currently only NonRec
  isTypeClassConstructor :: (CF.Bind CF.Ann) -> Bool
  isTypeClassConstructor (CF.NonRec (_, _, _, Just CF.IsTypeClassConstructor) id expr) = True
  isTypeClassConstructor _ = False


  findTypeClassFunctions :: [CF.Bind CF.Ann] -> String -> [String]
  findTypeClassFunctions binds typeclass = (map head . group . sort . concat) allFuncs
            where allFuncs = map getTypeClassFunctionNames (findTypeClassInstances binds typeclass)


  findTypeClassInstances :: [CF.Bind CF.Ann] -> String -> [CF.Bind CF.Ann]
  findTypeClassInstances binds typeclass = filter (flip isTypeClassInstance typeclass) binds


  -- #TODO currently only NonRec
  isTypeClassInstance :: (CF.Bind CF.Ann) -> String -> Bool
  isTypeClassInstance (CF.NonRec a id expr) tcid = checkExpr expr
            where checkExpr (CF.App _ (CF.Var _ constructorId) _ ) = (qualifiedIdentToUnqualifiedString constructorId) == tcid
                  checkExpr _ = False
  isTypeClassInstance _ _ = False

  -- #TODO currently only NonRec
  getTypeClassFunctionNames :: (CF.Bind CF.Ann) -> [String]
  getTypeClassFunctionNames (CF.NonRec _ _ (CF.App _  _ (CF.Literal _ (PSLit.ObjectLiteral obj)))) = map (\(id, _) -> (stripQuotes . show) id) obj
  getTypeClassFunctionNames _ = []

{-
  getTypeClassInstanceFunctionBinds :: (CF.Bind CF.Ann) -> [Bind]
  getTypeClassInstanceFunctionBinds = (sortBy bindSort) . (map createBind) . getDict
              where
                bindSort x y = compare (identifier x) (identifier y)
                createBind (id, expr) = Bind ((stripQuotes . show) id) (exprToIL emptyEnvironment expr) ConstantBind
                getDict (CF.NonRec _ _ (CF.App _  _ (CF.Literal _ (PSLit.ObjectLiteral dict)))) = dict
                -- #TODO temp environment
                emptyEnvironment = Environment "main" preludeImports [] [] [] []
-}
  getTypeClassInstanceFunctionBinds :: (CF.Bind CF.Ann) -> [(CF.Bind CF.Ann)]
  getTypeClassInstanceFunctionBinds = (sortBy bindSort) . (map createBind) . getInstanceDict
              where
                bindSort x y = compare (getBindIdentifier x) (getBindIdentifier y)
                --createBind (id, expr) = ((stripQuotes . show) id, expr)
                createBind (id, expr) = CF.NonRec nullAnn (PS.Ident (pack $ stripQuotes $ show id)) expr
                getInstanceDict (CF.NonRec _ _ (CF.App _  _ (CF.Literal _ (PSLit.ObjectLiteral dict)))) = dict
                nullAnn = (nullSourceSpan, [], Nothing, Nothing)

  getTypeClassInstanceExpressions :: (CF.Bind CF.Ann) -> [CF.Expr CF.Ann]
  getTypeClassInstanceExpressions = (map snd) . (sortBy keySort) . getInstanceDict
              where
                keySort (key1, _) (key2, _) =  compare key1 key2
                getInstanceDict (CF.NonRec _ _ (CF.App _  _ (CF.Literal _ (PSLit.ObjectLiteral dict))))
                  = map (\(key, expr) -> (show key, expr)) dict



  createTypeClassInstances :: [(CF.Bind CF.Ann)] -> TypeClass -> [TypeClassInstance]
  createTypeClassInstances binds typeclass =
    map (\bind -> Instance (instanceName bind) typeclass (getTypeClassInstanceExpressions bind))
        instanceBinds
                where instanceBinds = findTypeClassInstances binds (typeclassname typeclass)
                      instanceName (CF.NonRec _ id _) = identToString id


  removeTypClassBinds :: [TypeClass] -> [(CF.Bind CF.Ann)] -> [(CF.Bind CF.Ann)]
  removeTypClassBinds typeclasses binds = filter (\b -> not (isTypeClassConstructor b || isAccessor b)) binds
      where isTypeClassConstructor b = (getBindIdentifier b) `elem` (map typeclassname typeclasses)
            isAccessor b             = (getBindIdentifier b) `elem` (concat $ map typeclassfunctions typeclasses)


  removeInstanceBinds :: [TypeClassInstance] -> [(CF.Bind CF.Ann)] -> [(CF.Bind CF.Ann)]
  removeInstanceBinds instances binds = filter (\b -> not $ (getBindIdentifier b) `elem` (map instanceName instances)) binds