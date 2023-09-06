module Common.PureScriptUtility where

  import qualified Language.PureScript.Names as PS
  import qualified Language.PureScript.CoreFn as CF

  -- gets the name of a CoreFn module
  getModuleName :: CF.Module a -> String
  getModuleName = moduleNameToString . CF.moduleName

  moduleNameToString :: PS.ModuleName -> String
  moduleNameToString = stripQuotes . show . PS.runModuleName

  getBindIdentifier :: (CF.Bind CF.Ann) -> String
  getBindIdentifier (CF.NonRec _ id _) = identToString id
  -- #TODO not only use first bind of list of rec binds
  getBindIdentifier (CF.Rec binds)     = identToString id
      where ((_, id), _) = head binds

  -- many imported JSON keys contain quotes
  stripQuotes :: String -> String
  stripQuotes = filter (/= '\"')

  -- convert a PureScript identifier to a string
  identToString :: PS.Ident -> String
  identToString = stripQuotes . show . PS.runIdent

  -- currently only works with qualifiction by module 
  qualifiedIdentToUnqualifiedString :: PS.Qualified PS.Ident -> String
  qualifiedIdentToUnqualifiedString (PS.Qualified _ s) = identToString s

  qualifiedIdentToString :: PS.Qualified PS.Ident -> String
  qualifiedIdentToString = stripQuotes . show . (PS.showQualified PS.runIdent)
  --qualifiedIdentToString PS.Qualified (PS.ByModuleName mn) ident = (stripQuotes $ show $ PS.runModuleName mn) ++ "."
