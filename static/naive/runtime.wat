(module
  (type $closure_arg_array (array (mut (ref null eq))))
  (type $closure_top (struct (field (ref func))(field (ref $closure_arg_array))))
  (type $classDict (array (mut (ref null eq))))

  ;; boxed scalar types
  (type $boxedi32 (struct (field i32)))
  (type $boxedi64 (struct (field i64)))
  (type $boxedf32 (struct (field f32)))
  (type $boxedf64 (struct (field f64)))
  
  (type $func-param-clos (func (param (ref $closure_arg_array))(param (ref null eq))(result (ref null eq))))
  (type $func-clos (func (param (ref $closure_arg_array))(result (ref null eq))))



  ;; create copy of original array with an added variable ref
  (func $addToArgArray (export "addToArgArray")
    (param $arg-array (ref $closure_arg_array))
    (param $new-var (ref null eq))
    (result (ref $closure_arg_array))
    (local $i i32)
    (local $len i32)
    (local $new-arr (ref $closure_arg_array))
    
    ;; set $len to length of arg-array
    local.get $arg-array
    array.len
    local.set $len

    ;; init $i to 0
    i32.const 0
    local.set $i

    ;; create new array
    ref.null eq
    local.get $len
    i32.const 1
    i32.add
    array.new $closure_arg_array
    local.set $new-arr

    ;; create copy of original array
    local.get $len
    i32.const 0
    i32.ne
    (if
      (then
        (loop $arrayRunDown
          ;; copy element $i from orig to new array
          local.get $new-arr
          local.get $i
          (array.get $closure_arg_array (local.get $arg-array)(local.get $i))
          array.set $closure_arg_array
          
          ;; $i++
          local.get $i
          i32.const 1
          i32.add
          local.tee $i
          
          ;; loop if $i < $len
          local.get $len
          i32.lt_s
          br_if $arrayRunDown
        )
      )
    )

    ;; add new clos-var to new array
    local.get $new-arr
    local.get $i
    local.get $new-var
    array.set $closure_arg_array


    ;; place ref to new array on stack and cast to immutable
    local.get $new-arr
    ;;ref.cast (ref $closure_arg_array)
  )

    ;; gets a single ref from a closure
  (func $getSingleClosureArg (export "getSingleClosureArg") 
    (param $clos (ref $closure_arg_array))
    (param $index i32)
    (result (ref null eq))

    local.get 0
    local.get 1
    array.get $closure_arg_array
  )

  ;; creates a new closure
  (func $newClosure (export "newClosure") 
    (param $func (ref func))
    (param $clos (ref $closure_arg_array))
    (result (ref $closure_top))

    local.get 0
    local.get 1
    struct.new $closure_top
  )

  ;; applies a function(closure) on an argument
  (func $apply (export "apply") (param $clos (ref $closure_top)) (param $arg (ref null eq)) (result (ref null eq))
    local.get $clos                 ;; zet closure op de stack
    struct.get $closure_top 1
    ref.cast (ref $closure_arg_array)
    local.get $arg                  ;; zet argument op de stack
    local.get $clos                 
    struct.get $closure_top 0       ;; haal functie ref uit closure
    ref.cast (ref $func-param-clos)
    call_ref $func-param-clos              ;; roep die functie aan met $clos en $arg
  )

)