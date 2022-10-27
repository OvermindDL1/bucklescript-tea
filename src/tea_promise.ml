

let cmd promise tagger =
  let open Vdom in
  Tea_cmd.call (function callbacks -> 
      let _ = promise
              |> Js.Promise.then_ (function res ->
                match tagger res with
                | Some msg -> 
                  let () = !callbacks.enqueue msg in
                  Js.Promise.resolve ()
                | None -> Js.Promise.resolve ()
                )
      in
      ()
    )


let result promise msg =
  let open Vdom in
  Tea_cmd.call (function callbacks ->
      let enq result =
        !callbacks.enqueue (msg result)
      in
      let _ = promise
              |> Js.Promise.then_ (function res ->
                  let resolve = enq (Tea_result.Ok res) in
                  Js.Promise.resolve resolve
                )
              |> Js.Promise.catch (function err ->
                  let err_to_string err =
                    {j|$err|j} in
                  let reject = enq (Tea_result.Error (err_to_string err)) in
                  Js.Promise.resolve reject
                )
      in
      ()
    )
