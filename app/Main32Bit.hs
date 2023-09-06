module Main32Bit where

  import qualified Main as M
  import Common.CompileOptions

  compileOptions = CompileOptions Integer32 Float32

  main :: IO()
  main = M.mainCompile compileOptions