let focus id =
  Tea_cmd.call
    (fun _enqueue  ->
       let ecb _ =
         let element =
          Webapi.Dom.Document.getElementById Webapi.Dom.document id
          |. Belt.Option.flatMap (Webapi.Dom.HtmlElement.ofElement) in
         match element with
         | None  ->
           Js.log ("Attempted to focus a non-existant element of: ", id)
         | Some elem -> Webapi.Dom.HtmlElement.focus elem
       in
       (* One to get out of the current render frame*)
       let cb _ = ignore (Webapi.requestCancellableAnimationFrame ecb) in
       (* And another to properly focus *)
       ignore (Webapi.requestCancellableAnimationFrame cb);
       ())


