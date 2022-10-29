
type ('flags, 'model, 'msg) navigationProgram =
  { init : 'flags -> Web.Location.t -> 'model * 'msg Tea_cmd.t
  ; update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t
  ; view : 'model -> 'msg Vdom.t
  ; subscriptions : 'model -> 'msg Tea_sub.t
  ; shutdown : 'model -> 'msg Tea_cmd.t
  }


let notifier : (Web.Location.t -> unit) option ref = ref None

let notifyUrlChange () =
  match !notifier with
  | None -> ()
  | Some cb ->
    let location = Web.Location.get () in
    let () = cb location in
    ()


let subscribe tagger =
  let open Vdom in
  let enableCall callbacks =
    let notifyHandler location =
      callbacks.enqueue (tagger location) in
    let () = notifier := Some notifyHandler in
    let handler = fun _event ->
      notifyUrlChange () in
    let window = Webapi.Dom.window in
    let () = Webapi.Dom.Window.addPopStateEventListener window handler in
    fun () -> Webapi.Dom.Window.removePopStateEventListener window handler
  in Tea_sub.registration "navigation" enableCall



let replaceState url =
  let history = Webapi.Dom.history in
  let state = Webapi.Dom.History.state history in
  let _ = Webapi.Dom.History.replaceState history state "" url in
  ()


let pushState url =
  let history = Webapi.Dom.history in
  let state = Webapi.Dom.History.state history in
  let _ = Webapi.Dom.History.pushState history state "" url in
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


let go step =
  Tea_cmd.call (fun _enqueue ->
    let history = Webapi.Dom.history in
    let _ = Webapi.Dom.History.go history step in
    let () = notifyUrlChange () in
    ()
  )

let back step = go (-step)
let forward step = go step


let navigationProgram locationToMessage stuff =
    let init flag =
      stuff.init flag (Web.Location.get ()) in

    let subscriptions model =
      Tea_sub.batch
        [ subscribe locationToMessage
        ; stuff.subscriptions model
        ] in

    let open! Tea_app in
    program
      { init = init
      ; update = stuff.update
      ; view = stuff.view
      ; subscriptions = subscriptions
      ; shutdown = stuff.shutdown
      }
