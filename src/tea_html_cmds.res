let focus = id =>
  Tea_cmd.call(_enqueue => {
    let ecb = _ =>
      switch Js.Nullable.toOption(Web.Document.getElementById(id)) {
      | None => Js.log(("Attempted to focus a non-existant element of: ", id))
      | Some(elem) => Web.Node.focus(elem)
      }

    /* One to get out of the current render frame */
    let cb = _ => ignore(Web.Window.requestAnimationFrame(ecb))
    /* And another to properly focus */
    ignore(Web.Window.requestAnimationFrame(cb))
    ()
  })
