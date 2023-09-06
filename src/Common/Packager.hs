module Common.Packager where

  import Wasm.Wasm
  import Wasm.Printer

  import Data.List (intercalate, nub)
  import qualified Data.Text as T
  import qualified Data.Text.Encoding as T
  import qualified Data.Text.IO as T

  import System.FilePath
  import System.FilePath.Posix
  import System.FilePath.Find (find, (==?), always, fileName, extension)
  import System.Directory (createDirectoryIfMissing, getCurrentDirectory, copyFile)

  import Paths_pswasm

  createWasmPackage :: FilePath -> Module -> IO ()
  createWasmPackage currentDir wasmModule
      = do
        let wasmOutputDir = currentDir </> "wasm"
        writeWasmModule wasmOutputDir wasmModule
        writeInitScript wasmOutputDir wasmModule
        copyWatFiles currentDir wasmOutputDir wasmModule
        copyRuntimeFile wasmOutputDir
        

  writeWasmModule :: FilePath -> Module -> IO ()
  writeWasmModule wasmOutputDir wasmModule
    = do
        let printedWasm = printModule wasmModule
        let filePath = (joinPath [wasmOutputDir, (modName wasmModule)] ++ ".wat")
        createDirectoryIfMissing True $ takeDirectory filePath
        T.writeFile filePath $ T.pack printedWasm


  writeInitScript :: FilePath -> Module -> IO ()
  writeInitScript wasmOutputDir wasmModule
    = do
        let printedInitScript = printInitScript wasmModule
        let filePath = (joinPath [wasmOutputDir, (modName wasmModule)] ++ ".wast")
        createDirectoryIfMissing True $ takeDirectory filePath
        T.writeFile filePath $ T.pack printedInitScript

  copyWatFiles :: FilePath -> FilePath -> Module -> IO ()
  copyWatFiles currentDir wasmDir wasmModule
    = do
        files <- find always (extension ==? ".wat") (currentDir </> "src")
        let neededFileNames = map (\x -> x ++ ".wat") (neededImportFiles wasmModule)
        let filesToCopy = filter (\x -> (takeFileName x) `elem` neededFileNames) files
        mapM (\x -> copyFile x (wasmDir </> (takeFileName x))) filesToCopy
        print ""
  
  copyRuntimeFile :: FilePath -> IO ()
  copyRuntimeFile wasmOutputDir
    = do 
        runtimeFilePath <- getDataFileName "static/naive/runtime.wat"
        copyFile runtimeFilePath $ wasmOutputDir </> "runtime.wat"


  neededImportFiles :: Module -> [String]
  neededImportFiles wasmModule = nub $ (filter (/= "runtime") . map funcImpMod) (modImports wasmModule)