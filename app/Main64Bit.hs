module Main64Bit where

  import qualified Main as M
  import Common.CompileOptions

  compileOptions = CompileOptions Integer64 Float64

  main :: IO()
  main = M.mainCompile compileOptions