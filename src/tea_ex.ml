let render_event ?(key= "")  msg =
  let open Vdom in
    let enableCall callbacks =
      let () = callbacks.on ((AddRenderMsg (msg))[@explicit_arity ]) in
      fun () -> callbacks.on ((RemoveRenderMsg (msg))[@explicit_arity ]) in
    Tea_sub.registration key enableCall
module LocalStorage =
  struct
    open Tea_task
    open Tea_result
    let length =
      nativeBinding
        (fun cb ->
           match Web.Window.LocalStorage.length Web.Window.window with
           | None ->
               cb ((Error ("localStorage is not available"))
                 [@explicit_arity ])
           | ((Some (value))[@explicit_arity ]) ->
               cb ((Ok (value))[@explicit_arity ]))
    let clear =
      nativeBinding
        (fun cb ->
           match Web.Window.LocalStorage.clear Web.Window.window with
           | None ->
               cb ((Error ("localStorage is not available"))
                 [@explicit_arity ])
           | ((Some (value))[@explicit_arity ]) ->
               cb ((Ok (value))[@explicit_arity ]))
    let clearCmd () = Tea_task.attemptOpt (fun _ -> None) clear
    let key idx =
      nativeBinding
        (fun cb ->
           match Web.Window.LocalStorage.key Web.Window.window idx with
           | None ->
               cb ((Error ("localStorage is not available"))
                 [@explicit_arity ])
           | ((Some (value))[@explicit_arity ]) ->
               cb ((Ok (value))[@explicit_arity ]))
    let getItem key =
      nativeBinding
        (fun cb ->
           match Web.Window.LocalStorage.getItem Web.Window.window key with
           | None ->
               cb ((Error ("localStorage is not available"))
                 [@explicit_arity ])
           | ((Some (value))[@explicit_arity ]) ->
               cb ((Ok (value))[@explicit_arity ]))
    let removeItem key =
      nativeBinding
        (fun cb ->
           match Web.Window.LocalStorage.removeItem Web.Window.window key
           with
           | None ->
               cb ((Error ("localStorage is not available"))
                 [@explicit_arity ])
           | ((Some (value))[@explicit_arity ]) ->
               cb ((Ok (value))[@explicit_arity ]))
    let removeItemCmd key =
      Tea_task.attemptOpt (fun _ -> None) (removeItem key)
    let setItem key value =
      nativeBinding
        (fun cb ->
           match Web.Window.LocalStorage.setItem Web.Window.window key value
           with
           | None ->
               cb ((Error ("localStorage is not available"))
                 [@explicit_arity ])
           | ((Some (()))[@explicit_arity ]) ->
               cb ((Ok (()))[@explicit_arity ]))
    let setItemCmd key value =
      Tea_task.attemptOpt (fun _ -> None) (setItem key value)
  end