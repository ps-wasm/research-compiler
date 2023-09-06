module Common.CompileOptions where

  data CompileOptions = CompileOptions {
                            integerFormat :: IntegerFormat,
                            floatFormat   :: FloatFormat
                          } deriving (Show, Eq)

  data IntegerFormat = Integer31 | Integer32 | Integer64 deriving (Show, Eq)
  data FloatFormat   = Float32   | Float64 deriving (Show, Eq)

  defaultCompileOptions = CompileOptions Integer31 Float32