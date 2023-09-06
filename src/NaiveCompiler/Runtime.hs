module NaiveCompiler.Runtime where
  import Wasm.Wasm

  functionImports = [importFuncApply
                    ,importFuncAddToArgArray
                    ,importFuncGetSingleClosureArg
                    ,importFuncNewClosure]

  functionTypes   = [typeDescApply
                    ,typeDescAddToArgArray
                    ,typeDescNewClosure
                    ,typeDescGetSingleClosureArg
                    ,typeDescFuncParamClos
                    ,typeDescFuncClos]

  heapTypes       = [typeDescClosureArgArray
                    ,typeDescClosureTop
                    ,typeDescClassDict
                    ,typeDescBoxedi32
                    ,typeDescBoxedi64
                    ,typeDescBoxedf32
                    ,typeDescBoxedf64]

  typeIdFuncParamClos           = "runtime.func-param-clos"
  typeIdFuncParam               = "runtime.func-param"
  typeIdFuncClos                = "runtime.func-clos"
  typeIdFuncNoParamNoClos       = "runtime.func-noparam-noclos"
  typeIdFuncTop             = "runtime.func_top"
  typeIdClosureTop          = "runtime.closure_top"
  typeIdClosureArgArray     = "runtime.closure_arg_array"
  typeIdClassDict           = "runtime.classDict"
  typeIdBoxedi32            = "runtime.boxedi32"
  typeIdBoxedi64            = "runtime.boxedi64"
  typeIdBoxedf32            = "runtime.boxedf32"
  typeIdBoxedf64            = "runtime.boxedf64"

  funcIdApply               = "runtime.apply"
  funcIdAddToArgArray       = "runtime.addToArgArray"
  funcIdGetSingleClosureArg = "runtime.getSingleClosureArg"
  funcIdNewClosure          = "runtime.newClosure"
  funcIdBoxi64              = "box-i64"
  funcIdUnboxi64            = "unbox-i64"
  funcIdBoxf32              = "box-f32"
  funcIdUnboxf32            = "unbox-f32"
  funcIdBoxf64              = "box-f64"
  funcIdUnboxf64            = "unbox-f64"

  typeDescFuncTop = TypeDesc typeIdFuncTop
                      (FunctionType [RefType (Ref (HeapTypeDesc typeIdClosureArgArray)), RefType (Ref Eq)]
                                    [RefType (Ref Eq)])   -- not used anymore (?)
  typeDescApply   = TypeDesc funcIdApply
                      (FunctionType [RefType (Ref (HeapTypeDesc typeIdClosureTop)), RefType (NullableRef Eq)]
                                    [RefType (NullableRef Eq)])
  typeDescNewClosure = TypeDesc funcIdNewClosure
                         (FunctionType [RefType (Ref (Func ""))
                                       ,RefType (Ref (HeapTypeDesc typeIdClosureArgArray))]
                                       [RefType (Ref (HeapTypeDesc typeIdClosureTop))])
  typeDescAddToArgArray = TypeDesc funcIdAddToArgArray
                            (FunctionType [RefType (Ref (HeapTypeDesc typeIdClosureArgArray))
                                          ,RefType (NullableRef Eq)]
                                          [RefType (Ref (HeapTypeDesc typeIdClosureArgArray))])
  typeDescGetSingleClosureArg = TypeDesc funcIdGetSingleClosureArg
                                  (FunctionType [RefType (Ref (HeapTypeDesc typeIdClosureArgArray))
                                                ,NumType I32]
                                                [RefType (NullableRef Eq)])
  typeDescFuncParamClos = TypeDesc typeIdFuncParamClos
                            (FunctionType
                              [RefType (Ref (HeapTypeDesc typeIdClosureArgArray)), RefType (NullableRef Eq)]
                              [RefType (NullableRef Eq)])
  typeDescFuncParam     = TypeDesc typeIdFuncParam
                            (FunctionType
                              [RefType (NullableRef Eq)]
                              [RefType (NullableRef Eq)])
  typeDescFuncClos      = TypeDesc typeIdFuncClos
                            (FunctionType
                              [RefType (Ref (HeapTypeDesc typeIdClosureArgArray))]
                              [RefType (NullableRef Eq)])
  typeDescFuncNoParamNoClos      = TypeDesc typeIdFuncClos
                                     (FunctionType
                                       []
                                       [RefType (NullableRef Eq)])


  typeDescClosureTop = TypeDesc typeIdClosureTop (Struct [(False, RefType (Ref (Func ""))), (False, RefType (Ref (HeapTypeDesc typeIdClosureArgArray)))])
  typeDescClosureArgArray = TypeDesc typeIdClosureArgArray (Array True (RefType (NullableRef Eq)))
  typeDescClassDict = TypeDesc typeIdClassDict (Array True (RefType (NullableRef (Eq))))

  typeDescBoxedi32 = TypeDesc typeIdBoxedi32 (Struct [(False, NumType I32)])
  typeDescBoxedi64 = TypeDesc typeIdBoxedi64 (Struct [(False, NumType I64)])
  typeDescBoxedf32 = TypeDesc typeIdBoxedf32 (Struct [(False, NumType F32)])
  typeDescBoxedf64 = TypeDesc typeIdBoxedf64 (Struct [(False, NumType F64)])

  importFuncApply = FunctionImport "runtime" "apply" funcIdApply (TypeUse funcIdApply)
  importFuncNewClosure = FunctionImport "runtime" "newClosure" funcIdNewClosure (TypeUse funcIdNewClosure)
  importFuncAddToArgArray = FunctionImport "runtime" "addToArgArray" funcIdAddToArgArray (TypeUse funcIdAddToArgArray)
  importFuncGetSingleClosureArg = FunctionImport "runtime" "getSingleClosureArg" funcIdGetSingleClosureArg (TypeUse funcIdGetSingleClosureArg)