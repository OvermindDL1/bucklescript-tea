type 'msg applicationCallbacks = 'msg Vdom.applicationCallbacks
type 'msg t =
  | NoCmd: _ t 
  | Mapper:
  ('msg Vdom.applicationCallbacks ref -> 'msgB Vdom.applicationCallbacks ref)
  * 'msgB t -> 'msg t 
  | Batch: 'msg t list -> 'msg t 
  | EnqueueCall: ('msg applicationCallbacks ref -> unit) -> 'msg t 
let none = NoCmd
let batch cmds = ((Batch (cmds))[@explicit_arity ])
let call call = ((EnqueueCall (call))[@explicit_arity ])
let fnMsg fnMsg =
  let open Vdom in
    ((EnqueueCall ((fun callbacks -> (!callbacks).enqueue (fnMsg ()))))
    [@explicit_arity ])
let msg msg =
  let open Vdom in
    ((EnqueueCall ((fun callbacks -> (!callbacks).enqueue msg)))
    [@explicit_arity ])
let rec run : type msg. msg applicationCallbacks ref -> msg t -> unit =
  fun callbacks ->
    function
    | NoCmd -> ()
    | ((Mapper (mapper, cmd))[@implicit_arity ]) ->
        let subCallbacks = mapper callbacks in run subCallbacks cmd
    | ((Batch (cmds))[@explicit_arity ]) ->
        List.fold_left (fun () -> fun cmd -> run callbacks cmd) () cmds
    | ((EnqueueCall (cb))[@explicit_arity ]) -> cb callbacks
let map : type a b. (a -> b) -> a t -> b t =
  fun func ->
    fun cmd ->
      let mapper = Vdom.wrapCallbacks func in ((Mapper (mapper, cmd))
        [@implicit_arity ])