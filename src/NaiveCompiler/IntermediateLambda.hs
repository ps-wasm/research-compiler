module NaiveCompiler.IntermediateLambda where
  import Common.PureScriptUtility
  import Common.TypeClass

  import qualified Language.PureScript.CoreFn as CF
  import qualified Language.PureScript.AST.Literals as PSLit
  import qualified Language.PureScript.Names as PS

  import qualified Data.Text as T
  import Data.Either
  import Data.Maybe
  import Data.List


  data Bind = Bind {identifier :: String,
                    expression :: Expression,
                    kind       :: BindKind } deriving (Show, Eq)

  data BindKind = LiteralConst
                | FuncWithParamAndClosArgs
                | FuncWithParam
                | FuncWithClosArgs
                | FuncWithNoParamNoClosArgs
                | UnknownBind 
                  deriving (Show, Eq)
      

  data Expression = IntegerLiteral Integer -- *
                  | NumberLiteral Double   -- *
                  | BooleanLiteral Bool
                  | ArrayLiteral [Expression] -- *
                  | Get String -- *
                  | GetTypeClassElem Int Expression -- * #TODO! dit ook in traversals opnemen?
                  | GetClosureVar Int -- *
                  | GetGlobal String -- nodig?
                  | GetLocal Int -- *
                  | SetLocal String Expression
                --  | CallLambda String
                  | LambdaPlaceHolder Expression
                  | Apply Expression Expression -- *
                  | Lambda [String] Parameter Expression -- *
               -- | Case [case-expr] [[if-tests], case alt expr
                  | Let [Bind] Expression
                  | SingleBoolCase Expression Expression Expression
                  | Unknown String
                     deriving (Show, Eq)

  data Module = Module {
                  moduleName          :: String,
                  foreigns            :: [(String, String)],
                  declarations        :: [Bind],
                  exports             :: [String],
                  tc                  :: [TypeClass]
                } deriving Show
  
  data Environment = Environment {
                      nameSpace    :: [String],
                      locals       :: [String],
                      lets         :: [String],
                      closure      :: [String]
                    } deriving Show

   -- instance name, typeclass name, [function expressions]

  type Parameter = String

  --data CaseAlternative = CaseAlternative [Expression] [Expression]

  -- for now: as literals, later on: generated
  -- preludeImports = ["Data.Semiring.semiringInt", "Data.Semiring.semiringNumber", "Data.Semiring.mul", "Data.Semiring.add"]

  emptyEnvironment :: Environment
  emptyEnvironment = Environment [] [] [] []

  nameSpaceAddSuffix :: Environment -> String -> Environment
  nameSpaceAddSuffix env s = Environment ((nameSpace env)++[s]) (locals env) (lets env) (closure env)

  nameSpaceDropSuffix :: Environment -> Environment
  nameSpaceDropSuffix env = Environment (init $ nameSpace env) (locals env) (lets env) (closure env)

  getNameSpaceString :: Environment -> String
  getNameSpaceString = intercalate "." . nameSpace



  -- create accessor functions
  typeClassToIL :: Environment -> TypeClass -> [Bind]
  typeClassToIL env tc = map (\(n, func) -> Bind (ns ++ {-typeclassname tc ++ "." ++-} func)
                                             (Lambda [] paramID $ GetTypeClassElem n $ GetLocal 1 {-paramID-})
                                             FuncWithParam)
                          (zip [0..] (typeclassfunctions tc))
    where
      paramID = "tcArray"
      ns = (getNameSpaceString env) ++ "."

  -- creates:
  -- * IL for instance functions
  -- * IL for instance array containing refs to these funcs
  typeClassInstanceToIL :: Environment -> TypeClassInstance -> [Bind]
  typeClassInstanceToIL env inst = [funcRefArray] -- : instFuncs
    where
      {-
      instFuncs    = createIL (nameSpaceAddSuffix env instName) (instanceBinds inst)
      funcRefArray = Bind (ns ++ (takeWhile (/= '$') tcName) ++ "." ++ instName)
                     (ArrayLiteral $ map (Get . identifier) instFuncs)
                     LiteralConst
      -}
      funcRefArray = Bind (ns ++ {-(takeWhile (/= '$') tcName) ++ "." ++-} instName)
                          (ArrayLiteral (map (exprToIL env) (instanceExpressions inst)))
                          LiteralConst
      tcName       = (typeclassname $ instanceTypeclass inst)
      instName     = instanceName inst
      ns           = (getNameSpaceString env) ++ "."


  -- given an environment, create intermediary binds from coreFn binds
  createIL :: Environment -> [CF.Bind CF.Ann] -> [Bind]
  createIL env = map (bindToIL env)


  -- create a single intermediary bind
  bindToIL :: Environment -> CF.Bind CF.Ann -> Bind
  bindToIL env (CF.NonRec a id expr) = Bind identifier expression UnknownBind
    where identifier  = ns ++ (identToString id)
          expression  = (exprToIL env expr)
          ns = (getNameSpaceString env) ++ "."
  -- #TODO! not only use first bind of list of rec binds
  bindToIL env (CF.Rec binds)        = Bind identifier expression UnknownBind
    where ((_, id), expr) = head binds
          identifier  = ns ++ (identToString id)
          expression  = (exprToIL env expr)
          ns = (getNameSpaceString env) ++ "."


  -- convert CoreFn expression to intermediary expression
  exprToIL :: Environment -> CF.Expr CF.Ann -> Expression
  exprToIL env (CF.App a expr1 expr2) = Apply (exprToIL env expr1) (exprToIL env expr2)
  exprToIL env (CF.Literal a (PSLit.NumericLiteral (Left x)))  = IntegerLiteral x
  exprToIL env (CF.Literal a (PSLit.NumericLiteral (Right x))) = NumberLiteral x
  exprToIL env (CF.Literal a (PSLit.BooleanLiteral x))         = BooleanLiteral x
  exprToIL env (CF.Var a (PS.Qualified q ident))
        | id `elem` (locals env)    = GetLocal 1 {-id-}--(fromJust $ elemIndex id (locals env))     -- local 0 is closure maar regelen we later. TODO: eigenlijk is het altijd local 1 in wasm
        | id `elem` (lets env)      = Get ((getNameSpaceString env) ++ "." ++ id)
        | id `elem` (closure env)   = GetClosureVar (fromJust $ elemIndex id (closure env)) -- closure 0 is funcref maar regelen we later
        | otherwise = Get $ id
            where id = (prefix q) ++ (identToString ident)
                  prefix :: (PS.QualifiedBy) -> String
                  prefix (PS.ByModuleName mn) = (stripQuotes $ show $ PS.runModuleName mn) ++ "."
                  prefix _  = ""
  exprToIL env (CF.Abs _ id expr) = Lambda (closure newEnv) paramID (exprToIL newEnv expr)
            where newEnv  = environmentAddParam paramID $ environmentMoveParamToClos env
                  paramID = identToString id
  -- PureScript zorgt zelf dat er geen collision is tussen let binds en top binds
  exprToIL env (CF.Let _ binds expr)
    = Let (createIL newEnv binds)
          (exprToIL newEnv expr)
      where
        newEnv = (environmentMoveParamToClos $ addLetsToEnvironment env (map getBindIdentifier binds))
  exprToIL _ (CF.Constructor _ _ _ _)
    = Unknown "Constructor"
  exprToIL _ (CF.ObjectUpdate _ _ _)
    = Unknown "ObjectUpdate"
  exprToIL env (CF.Case
                  _
                  [testExpr]
                  [(CF.CaseAlternative [(CF.LiteralBinder _ (CF.BooleanLiteral True))] (Right ifTrueExpr))
                  ,(CF.CaseAlternative [(CF.NullBinder _)] (Right ifFalseExpr))
                  ]
               )
    = SingleBoolCase (exprToIL env testExpr) (exprToIL env ifTrueExpr) (exprToIL env ifFalseExpr)
  exprToIL _ _ = Unknown "?"

{-
  -- creates the environment for a lambda, based on the previous environment
  lambdaEnvironment :: Environment -> String -> Environment
  lambdaEnvironment (Environment ns (param:ls) lets clos) id = Environment ns (id:ls) lets (param:clos)
  lambdaEnvironment (Environment ns [] lets clos) id         = Environment ns [id] lets clos
  lambdaEnvironment env _                                    = env
-}
  environmentAddParam:: String -> Environment -> Environment
  environmentAddParam newParam (Environment ns param lets clos) = Environment ns (newParam:param) lets clos

  environmentMoveParamToClos :: Environment -> Environment
  environmentMoveParamToClos (Environment ns (param:ls) lets clos)
    = Environment ns ls lets (clos ++ [param])
  environmentMoveParamToClos env
    = env

  addLetsToEnvironment :: Environment -> [String] -> Environment
  addLetsToEnvironment (Environment ns locs lets clos) l = Environment ns locs (lets++l) clos


  -- lifts all lambdas to a global scope
  liftLambdas :: [Bind] -> [Bind]
  liftLambdas (x:xs) =
      [Bind bindID (replacePlaceholderCalls newExpr newBinds) (kind x)] ++ updatedBinds ++ (liftLambdas xs) 
            where (newExpr, newFuncs) = extractLambdas (expression x)
                  bindID              = identifier x
                  newBinds            = (bindsForLiftedLambdas bindID 1 newFuncs)
                  updatedBinds        = map (\x -> Bind (identifier x) (replacePlaceholderCalls (expression x) newBinds) (kind x)) newBinds
  liftLambdas []     = []


  -- creates a bind (name) for a lifted lambda
  bindsForLiftedLambdas :: String -> Int -> [Expression] -> [Bind]
  bindsForLiftedLambdas ident n (x:xs) =  newBind : bindsForLiftedLambdas ident (n+1) xs
            where newBind = (Bind (ident ++ "-aux-" ++ (show n)) x FuncWithParamAndClosArgs)
  bindsForLiftedLambdas _ _ [] = []


  -- finds non-toplevel lambdas in an expression and replace them with a temp placeholder calls
  extractLambdas :: Expression -> (Expression, [Expression])
  extractLambdas (Apply e1 e2)          = ((Apply ee1 ee2), ff1++ff2)
                      where (ee1, ff1) = extractLambdas e1
                            (ee2, ff2) = extractLambdas e2
  extractLambdas (Lambda (c:cs) p expr) = (LambdaPlaceHolder newExpr, [newExpr] ++ newFunc)
                      where (ee, newFunc) = extractLambdas expr
                            newExpr = Lambda (c:cs) p ee
  -- an empty closure means a toplevel lambda
  extractLambdas (Lambda [] p expr)     = ((Lambda [] p newExpr), newFunc)
                      where (newExpr, newFunc) = extractLambdas expr

{-
       if   (expression bind) == f
                                            then (Lambda c p newExpr, newFunc)
                                            else ((LambdaPlaceHolder f), [f])
                      where (newExpr, newFunc) = extractLambdas bind expr
-}
  -- leafs:
  extractLambdas expr              = (expr, [])


  -- after lifting and creating a new binding, replace placeholders
  -- with a correct call
  replacePlaceholderCalls :: Expression -> [Bind] -> Expression
  replacePlaceholderCalls (LambdaPlaceHolder expr) binds = {-CallLambda-}Get correctIdentifier 
                      -- TODO needs error handling in case of Nothing:
                      where correctIdentifier = identifier $ fromJust $ find (\x -> (expression x) == expr) binds
  replacePlaceholderCalls (Apply e1 e2) binds = (Apply ee1 ee2)
                      where ee1 = replacePlaceholderCalls e1 binds
                            ee2 = replacePlaceholderCalls e2 binds
  replacePlaceholderCalls (Lambda c p expr) binds = Lambda c p (replacePlaceholderCalls expr binds)
  replacePlaceholderCalls expr _ = expr 

  liftLetBinds :: [Bind] -> [Bind]
  liftLetBinds ((Bind id expr kind):binds)
    = ((Bind id newExpr kind) : newBinds) ++ (liftLetBinds binds)
      where (newExpr, newBinds) = extractLetBinds expr
  liftLetBinds [] = []

  extractLetBinds :: Expression -> (Expression, [Bind])
  extractLetBinds (Apply e1 e2) 
    = (Apply ee1 ee2, binds1++binds2)
      where (ee1, binds1) = extractLetBinds e1 
            (ee2, binds2) = extractLetBinds e2
  extractLetBinds (Lambda clos param e)
    = (Lambda clos param ee, binds)
      where (ee, binds)   = extractLetBinds e
  extractLetBinds (Let binds e)
    = (e, liftLetBinds binds)
  extractLetBinds e = (e, [])

  replaceUnknownBindKinds :: [Bind] -> [Bind]
  replaceUnknownBindKinds ((Bind id expr UnknownBind) : xs)
    = (Bind id expr (determineKind expr)) : replaceUnknownBindKinds xs
  replaceUnknownBindKinds ( x : xs )
    = (x : replaceUnknownBindKinds xs)
  replaceUnknownBindKinds []
    = []


  determineKind :: Expression -> BindKind
  determineKind (IntegerLiteral _ ) = LiteralConst
  determineKind (NumberLiteral _ )  = LiteralConst
  determineKind (ArrayLiteral _ )   = LiteralConst -- TODO! of zou [expr] clos of param nodig kunnen hebben?
  determineKind (Lambda _ _ e)
                   | expressionUsesClosure e = FuncWithParamAndClosArgs
                   | otherwise               = FuncWithParam
  determineKind e  | expressionUsesClosure e = FuncWithClosArgs
                   | otherwise               = FuncWithNoParamNoClosArgs


  expressionUsesClosure :: Expression -> Bool
  expressionUsesClosure (GetClosureVar _)             = True
  expressionUsesClosure (Apply e1 e2)                 = expressionUsesClosure e1
                                                        || expressionUsesClosure e2
  expressionUsesClosure (Let _ e)                     = expressionUsesClosure e
  expressionUsesClosure (SingleBoolCase e1 e2 e3)     = expressionUsesClosure e1
                                                        || expressionUsesClosure e2
                                                        || expressionUsesClosure e3
  expressionUsesClosure _                             = False

  --old:
  --determineKind IntegerLiteral      = EvalConst
  --determineKind NumberLiteral       = EvalConst
  --determineKind ArrayLiteral        = EvalConst
  --

