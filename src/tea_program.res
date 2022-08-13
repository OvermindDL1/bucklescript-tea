type processMsg<'msg> =
  | PushMsg('msg)
  | Kill

let spawn = (initState, update, shutdown) => {
  let state = ref(Some(initState))
  let onMessage = procMsg =>
    switch state.contents {
    | None => ()
    | Some(model) =>
      switch procMsg {
      | PushMsg(msg) =>
        let () = state := update(model, msg)
      | Kill =>
        let () = shutdown(model)
        let () = state := None
      }
    }
  onMessage
}

/* let testing0 =
  let s = spawn 42 (fun model -> let () = Js.log model in function
      | `Inc -> Some (model + 1)
      | `Dec -> Some (model - 1)
    ) (fun _ -> ()) in
  let () = s (PushMsg `Dec) in
  let () = s (PushMsg `Dec) in
  let () = s Kill in
  let () = s (PushMsg `Dec) in
  () */

module type Process = {
  /* This module should be `import`ed into a module that will become a persistent process.
     That process should have a handleMsg callback to handle its own message types.
     It should call itself
 */

  type msg

  let handleMsg: msg => unit
}

let testing1 = 42
