let focus = id =>
  Tea_cmd.call(_enqueue =>
    switch (Js.Nullable.toOption(Web.Document.getElementById(id))) {
    | None => Js.log(("Attempted to focus a non-existant element of: ", id))
    | Some(elem) =>
      /* let () = Js.log ("Focusing element", id, elem) in */
      let ecb = _ignored => Web.Node.focus(elem);
      let cb = _ignored => {
        let _unhandledID = Web.Window.requestAnimationFrame(ecb);
        ();
      }; /* One to get out of the current render frame*/
      let _unhandledID = Web.Window.requestAnimationFrame(cb); /* And another to properly focus */
      ();
    }
  );
