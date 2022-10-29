let render_event ?(key= "")  msg =
  let open Vdom in
    let enableCall callbacks =
      let () = callbacks.on ((AddRenderMsg (msg))[@explicit_arity ]) in
      fun () -> callbacks.on ((RemoveRenderMsg (msg))[@explicit_arity ]) in
    Tea_sub.registration key enableCall


module LocalStorage = struct
  let nativeBinding = Tea_task.nativeBinding
  let ls = Dom.Storage.localStorage

  let length : (int, string) Tea_task.t = nativeBinding (fun cb -> cb (Ok (Dom.Storage2.length ls)))

  let clear : (unit,string) Tea_task.t = nativeBinding (fun cb -> cb (Ok (Dom.Storage2.clear ls)))

  let clearCmd () = Tea_task.attemptOpt (fun _ -> None) clear

  let key idx : (string option, string) Tea_task.t = nativeBinding (fun cb -> cb (Ok (Dom.Storage2.key ls idx)))

  let getItem key : (string option, string) Tea_task.t = nativeBinding (fun cb -> cb (Ok (Dom.Storage2.getItem ls key)))

  let removeItem key : (unit,string) Tea_task.t = nativeBinding (fun cb -> cb (Ok (Dom.Storage2.removeItem ls key)))

  let removeItemCmd key =
    Tea_task.attemptOpt (fun _ -> None) (removeItem key)

  let setItem key value : (unit,string) Tea_task.t = nativeBinding (fun cb -> cb (Ok (Dom.Storage2.setItem ls key value)))

  let setItemCmd key value =
    Tea_task.attemptOpt (fun _ -> None) (setItem key value)

end