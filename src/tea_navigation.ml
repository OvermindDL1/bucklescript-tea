let getLocation () =
  Web.Location.asRecord (Web.Document.location ())



let notifier : (Web.Location.location -> unit) option ref = ref None

let notifyUrlChange () =
  match !notifier with
  | None -> ()
  | Some cb ->
    let location = getLocation () in
    let () = cb location in
    ()


let subscribe tagger =
  let open Vdom in
  let enableCall callbacks =
    let notifyHandler location =
      callbacks.enqueue (tagger location) in
    let () = notifier := Some notifyHandler in
    let handler : Web.Node.event_cb = fun [@bs] _event ->
      notifyUrlChange () in
    let () = Web.Window.addEventListener "popstate" handler false in
    fun () -> Web.Window.removeEventListener "popstate" handler false
  in Tea_sub.registration "navigation" enableCall



let replaceState url =
  let _ = Web.Window.History.replaceState Web.Window.window (Js.Json.parseExn "{}") "" url in
  ()


let pushState url =
  let _ = Web.Window.History.pushState Web.Window.window (Js.Json.parseExn "{}") "" url in
  ()


let modifyUrl url =
  Tea_cmd.call (fun _enqueue ->
      let () = replaceState url in
      let () = notifyUrlChange () in
      ()
    )


let newUrl url =
  Tea_cmd.call (fun _enqueue ->
      let () = pushState url in
      let () = notifyUrlChange () in
      ()
    )

module type NavigationProgram = sig
  type flags
  type model
  type msg
  val init : flags -> Web.Location.location -> model * msg Tea_cmd.t
  val update : model -> msg -> model * msg Tea_cmd.t
  val view : model -> msg Vdom.t
  val subscriptions : model -> msg Tea_sub.t
  val shutdown : model -> msg Tea_cmd.t
  val locationHandler : Web.Location.location -> msg
end

module ToProgram (M : NavigationProgram) : Tea_app.Program = struct
  type msg = M.msg
  type model = M.model
  type flags = M.flags
  let subscriptions model = Tea_sub.batch
        [ subscribe M.locationHandler
        ; M.subscriptions model
        ]

  let init flags =
    M.init flags (getLocation ())

  let update = M.update
  let view = M.view
  let shutdown = M.shutdown
end

module MakeNavigationProgram( M: NavigationProgram ) =
  Tea_app.Make(ToProgram(M))
