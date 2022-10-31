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
  let ls = Dom.Storage.localStorage

  let length: Tea_task.t<int, string> = nativeBinding(cb => cb(Ok(Dom.Storage2.length(ls))))

  let clear: Tea_task.t<unit, string> = nativeBinding(cb => cb(Ok(Dom.Storage2.clear(ls))))

  let clearCmd = () => Tea_task.attemptOpt(_ => None, clear)

  let key = (idx): Tea_task.t<option<string>, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.key(ls, idx))))

  let getItem = (key): Tea_task.t<option<string>, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.getItem(ls, key))))

  let removeItem = (key): Tea_task.t<unit, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.removeItem(ls, key))))

  let removeItemCmd = key => Tea_task.attemptOpt(_ => None, removeItem(key))

  let setItem = (key, value): Tea_task.t<unit, string> =>
    nativeBinding(cb => cb(Ok(Dom.Storage2.setItem(ls, key, value))))

  let setItemCmd = (key, value) => Tea_task.attemptOpt(_ => None, setItem(key, value))
}
