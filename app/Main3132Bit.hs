module Main3132Bit where

  import qualified Main as M
  import Common.CompileOptions

  compileOptions = CompileOptions Integer31 Float32

  main :: IO()
  main = M.mainCompile compileOptions