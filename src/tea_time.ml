

type t = float


(* type 'msg mySub =
  | Every of t * (t -> 'msg)


type 'msg myCmd =
  | Delay of t * (unit -> 'msg) *)


let every ~key interval tagger =
  let open Vdom in
  let enableCall callbacks =
    let id = (Js.Global.setInterval (fun () -> callbacks.enqueue (tagger (Js.Date.now ())) ) interval) in
    fun () -> Js.Global.clearInterval id
  in Tea_sub.registration key enableCall


let delay msTime msg =
  Tea_cmd.call
    ( fun callbacks ->
        let _unhandledID =
          Js.Global.setTimeout
            ( fun () ->
                let open Vdom in
                !callbacks.enqueue msg
            )
            msTime
        in ()
    )


(* Generic Helpers *)

let millisecond = 1.0


let second =
  1000.0 *. millisecond


let minute =
  60.0 *. second


let hour =
  60.0 *. minute


let inMilliseconds t =
  t


let inSeconds t =
  t /. second


let inMinutes t =
  t /. minute


let inHours t =
  t /. hour
