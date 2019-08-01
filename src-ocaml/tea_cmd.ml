

type 'msg applicationCallbacks = 'msg Vdom.applicationCallbacks

type 'msg t =
  | NoCmd : _ t
  | Mapper : ('msg Vdom.applicationCallbacks ref -> 'msgB Vdom.applicationCallbacks ref) * 'msgB t -> 'msg t
  | Batch : 'msg t list -> 'msg t
  | EnqueueCall : ('msg applicationCallbacks ref -> unit) -> 'msg t



let none = NoCmd


let batch cmds =
  Batch cmds


let call call =
  EnqueueCall call


let fnMsg fnMsg =
  let open Vdom in
  EnqueueCall (fun callbacks -> !callbacks.enqueue (fnMsg ()))


let msg msg =
  let open Vdom in
  EnqueueCall (fun callbacks -> !callbacks.enqueue msg)


let rec run : type msg . msg applicationCallbacks ref -> msg t -> unit = fun callbacks ->
  function
  | NoCmd -> ()
  | Mapper (mapper, cmd) ->
    let subCallbacks = mapper callbacks in
    run subCallbacks cmd
  | Batch cmds -> List.fold_left (fun () cmd -> run callbacks cmd) () cmds
  | EnqueueCall cb ->
    (* let () = Js.log ("Cmd.run", "enqueue", cb) in *)
    cb callbacks



(* let wrapCallbacks func callbacks = *)
(*   let open Vdom in *)
(*   ref *)
(*     { enqueue = (fun msg -> !callbacks.enqueue (func msg)) *)
(*     } *)

let map : type a b .(a -> b) -> a t -> b t = fun func cmd ->
  let mapper = Vdom.wrapCallbacks func in
  Mapper (mapper, cmd)
