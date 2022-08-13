let render_event = (~key="", msg) => {
  open Vdom
  let enableCall = callbacks => {
    let () = callbacks.on(AddRenderMsg(msg))
    () => callbacks.on(RemoveRenderMsg(msg))
  }
  Tea_sub.registration(key, enableCall)
}
module LocalStorage = {
  open Tea_task
  open Tea_result
  let length = nativeBinding(cb =>
    switch Web.Window.LocalStorage.length(Web.Window.window) {
    | None => cb(Error("localStorage is not available"))
    | Some(value) => cb(Ok(value))
    }
  )
  let clear = nativeBinding(cb =>
    switch Web.Window.LocalStorage.clear(Web.Window.window) {
    | None => cb(Error("localStorage is not available"))
    | Some(value) => cb(Ok(value))
    }
  )
  let clearCmd = () => Tea_task.attemptOpt(_ => None, clear)
  let key = idx =>
    nativeBinding(cb =>
      switch Web.Window.LocalStorage.key(Web.Window.window, idx) {
      | None => cb(Error("localStorage is not available"))
      | Some(value) => cb(Ok(value))
      }
    )
  let getItem = key =>
    nativeBinding(cb =>
      switch Web.Window.LocalStorage.getItem(Web.Window.window, key) {
      | None => cb(Error("localStorage is not available"))
      | Some(value) => cb(Ok(value))
      }
    )
  let removeItem = key =>
    nativeBinding(cb =>
      switch Web.Window.LocalStorage.removeItem(Web.Window.window, key) {
      | None => cb(Error("localStorage is not available"))
      | Some(value) => cb(Ok(value))
      }
    )
  let removeItemCmd = key => Tea_task.attemptOpt(_ => None, removeItem(key))
  let setItem = (key, value) =>
    nativeBinding(cb =>
      switch Web.Window.LocalStorage.setItem(Web.Window.window, key, value) {
      | None => cb(Error("localStorage is not available"))
      | Some() => cb(Ok())
      }
    )
  let setItemCmd = (key, value) => Tea_task.attemptOpt(_ => None, setItem(key, value))
}
