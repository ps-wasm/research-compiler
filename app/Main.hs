module Main (mainCompile) where

import Common.PureScriptUtility
import Common.TypeClass
import Common.Packager
import Common.CompileOptions

--import NaiveCompiler.Module
import Wasm.Printer
import Wasm.Wasm
import NaiveCompiler.WasmCompiler
import qualified NaiveCompiler.IntermediateLambda as IL
--import qualified NaiveCompiler.CoreWasm as CW

import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.CoreFn.FromJSON

import Data.Aeson
import Data.Aeson.Types

import Data.List (partition, isPrefixOf, (\\), intercalate)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B

import System.Environment
import System.Process
import System.FilePath
import System.FilePath.Posix
import System.FilePath.Find (find, (==?), always, fileName, extension)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, copyFile)




mainCompile :: CompileOptions -> IO ()
mainCompile compileOptions = do
  --args <- getArgs
  --let (opts, moduleName, argfiles) = splitArgs args
  --    opts' = (map . map) toLower opts


  --print opts
  currentDir <- getCurrentDirectory
  files <- find always (fileName ==? "corefn.json") currentDir
  modules <- mapM (createCoreFnModule) files
  
  let ilModules = map toIntermediateModule modules
  --print ilModules
  
  let wasmModule = createWasmModule compileOptions ilModules
  createWasmPackage currentDir wasmModule
  
  print "Compilation succeeded"


toIntermediateModule :: CF.Module CF.Ann -> IL.Module
toIntermediateModule m = IL.Module moduleName foreigns intermediate exports typeClasses
  where
    moduleName     = getModuleName m
    foreigns       = map (\x-> (moduleName, identToString x)) (CF.moduleForeign m)
    exports        = map (addNameSpace . identToString) (CF.moduleExports m)
    corefnDecls    = CF.moduleDecls m
    typeClasses    = findTypeClasses corefnDecls
    typeInstances  = concat $ map (createTypeClassInstances corefnDecls) typeClasses
    declsToCompile = ((removeInstanceBinds typeInstances) $ (removeTypClassBinds typeClasses) corefnDecls)
    intermediate   = IL.replaceUnknownBindKinds
                     $ IL.liftLambdas
                     $ IL.liftLetBinds
                     $ ((IL.createIL environment declsToCompile)
                       ++ (concat $ map (IL.typeClassToIL environment) typeClasses)
                       ++ (concat $ map (IL.typeClassInstanceToIL environment) typeInstances))
    environment    = IL.nameSpaceAddSuffix IL.emptyEnvironment moduleName
    addNameSpace   = ((++) (moduleName++"."))


createCoreFnModule :: FilePath -> IO (CF.Module CF.Ann)
createCoreFnModule jsonFile = do jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
                                 return $ jsonToModule $ parseJson jsonText

getModuleImports :: CF.Module a -> [String]
getModuleImports (CF.Module _ _ _ _ imp _ _ _ _) = map (moduleNameToString . snd) imp

getModuleForeigns :: CF.Module a -> [String]
getModuleForeigns (CF.Module _ _ _ _ _ _ _ foreigns _) = map identToString foreigns

{-
removePrimDependency :: CF.Module a -> CF.Module a
removePrimDependency (CF.Module ss comm name path imp exp reexp forgn decl)
  = CF.Module ss comm name path newImp exp reexp forgn decl
      where newImp = filter ((flip notElem ["Prim",(moduleNameToString name)]) . moduleNameToString . snd) imp
-}              

-- The functions 'parseJson' and 'jsonToModule' are copied from Andy Arvanitis'
-- purescipt -> native compiler, see https://github.com/andyarvanitis/purescript-native/blob/golang/app/Main.hs
parseJson :: T.Text -> Value
parseJson text
  | Just fileJson <- decode . L.encodeUtf8 $ L.fromStrict text = fileJson
  | otherwise = error "Bad json"


jsonToModule :: Value -> CF.Module CF.Ann
jsonToModule value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> error "failed"

splitArgs :: [String] -> ([String], Maybe String, [String])
splitArgs args =
  let (opts, files) = partition (isPrefixOf "--") args
      opts' = (map . map) toLower opts
      (moduleName, files') =
        if null files
          then (Nothing, files)
          else
            if "--run" `elem` opts' && length (files) >= 1
              then do
                let file0 = head files
                    files' = tail files
                case file0 of
                  "Main.main" -> (Just "Main", files')
                  "Test.Main.main" -> (Just "Test.Main", files')
                  _ -> (Nothing, files)
              else (Nothing, files)
    in (opts, moduleName, files')