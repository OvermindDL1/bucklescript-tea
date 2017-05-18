
(* Everything here is not in Elm and is purely used as an extension and may vanish at any time if a better API comes out. *)

module LocalStorage = struct
  open Tea_task
  open Tea_result

  let getItem key =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.getItem Web.Window.window key with
        | None -> cb (Error "Key not found")
        | Some value -> cb (Ok value)
      )

  let setItem key value =
    nativeBinding (fun cb ->
        Web.Window.LocalStorage.setItem Web.Window.window key value
      )
end
