let render_event = (~key="", msg) => {
  open Vdom
  let enableCall = callbacks => {
    let () = callbacks.on(AddRenderMsg(msg))
    () => callbacks.on(RemoveRenderMsg(msg))
  }
  Tea_sub.registration(key, enableCall)
}

module LocalStorage = {
  let nativeBinding = Tea_task.nativeBinding

  let length: Tea_task.t<int, string> = nativeBinding(cb =>
    cb(Ok(Dom.Storage2.length(Dom.Storage.localStorage)))
  )

  let clear: Tea_task.t<unit, string> = nativeBinding(cb =>
    cb(Ok(Dom.Storage2.clear(Dom.Storage.localStorage)))
  )

  let clearCmd = () => Tea_task.attemptOpt(_ => None, clear)

  let key = (idx): Tea_task.t<option<string>, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.key(Dom.Storage.localStorage, idx))))

  let getItem = (key): Tea_task.t<option<string>, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.getItem(Dom.Storage.localStorage, key))))

  let removeItem = (key): Tea_task.t<unit, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.removeItem(Dom.Storage.localStorage, key))))

  let removeItemCmd = key => Tea_task.attemptOpt(_ => None, removeItem(key))

  let setItem = (key, value): Tea_task.t<unit, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.setItem(Dom.Storage.localStorage, key, value))))

  let setItemCmd = (key, value) => Tea_task.attemptOpt(_ => None, setItem(key, value))
}
