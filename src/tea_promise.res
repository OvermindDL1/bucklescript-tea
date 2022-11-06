let cmd = (promise, tagger) => {
  open Vdom
  Tea_cmd.call(callbacks => {
    let _ = promise |> Js.Promise.then_(res =>
      switch tagger(res) {
      | Some(msg) =>
        let () = callbacks.contents.enqueue(msg)
        Js.Promise.resolve()
      | None => Js.Promise.resolve()
      }
    )
  })
}

let result = (promise, msg) => {
  open Vdom
  Tea_cmd.call(callbacks => {
    let enq = result => callbacks.contents.enqueue(msg(result))

    let _ =
      promise
      |> Js.Promise.then_(res => {
        let resolve = enq(Ok(res))
        Js.Promise.resolve(resolve)
      })
      |> Js.Promise.catch(err => {
        let err_to_string = err => j`$err`
        let reject = enq(Error(err_to_string(err)))
        Js.Promise.resolve(reject)
      })
  })
}
