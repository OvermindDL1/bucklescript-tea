type 'msg t =
  | NoSub: _ t 
  | Batch: 'msg t list -> 'msg t 
  | Registration: string *
  ('msg Vdom.applicationCallbacks ref -> unit -> unit) * (unit -> unit)
  option ref -> 'msg t 
  | Mapper:
  ('msg Vdom.applicationCallbacks ref -> 'msgB Vdom.applicationCallbacks ref)
  * 'msgB t -> 'msg t 
type 'msg applicationCallbacks = 'msg Vdom.applicationCallbacks
let none = NoSub
let batch subs = ((Batch (subs))[@explicit_arity ])
let registration key enableCall =
  ((Registration
      (key, (fun callbacks -> enableCall (!callbacks)), (ref None)))
  [@implicit_arity ])
let map msgMapper sub =
  let func callbacks = Vdom.wrapCallbacks msgMapper callbacks in
  ((Mapper (func, sub))[@implicit_arity ])
let mapFunc func sub = ((Mapper (func, sub))[@implicit_arity ])
let rec run : type msgOld msgNew.
  msgOld Vdom.applicationCallbacks ref ->
    msgNew Vdom.applicationCallbacks ref -> msgOld t -> msgNew t -> msgNew t
  =
  fun oldCallbacks ->
    fun newCallbacks ->
      fun oldSub ->
        fun newSub ->
          let rec enable : type msg.
            msg Vdom.applicationCallbacks ref -> msg t -> unit =
            fun callbacks ->
              function
              | NoSub -> ()
              | ((Batch ([]))[@explicit_arity ]) -> ()
              | ((Batch (subs))[@explicit_arity ]) ->
                  List.iter (enable callbacks) subs
              | ((Mapper (mapper, sub))[@implicit_arity ]) ->
                  let subCallbacks = mapper callbacks in
                  enable subCallbacks sub
              | ((Registration (_key, enCB, diCB))[@implicit_arity ]) ->
                  diCB := ((Some ((enCB callbacks)))[@explicit_arity ]) in
          let rec disable : type msg.
            msg Vdom.applicationCallbacks ref -> msg t -> unit =
            fun callbacks ->
              function
              | NoSub -> ()
              | ((Batch ([]))[@explicit_arity ]) -> ()
              | ((Batch (subs))[@explicit_arity ]) ->
                  List.iter (disable callbacks) subs
              | ((Mapper (mapper, sub))[@implicit_arity ]) ->
                  let subCallbacks = mapper callbacks in
                  disable subCallbacks sub
              | ((Registration (_key, _enCB, diCB))[@implicit_arity ]) ->
                  (match !diCB with
                   | None -> ()
                   | ((Some (cb))[@explicit_arity ]) ->
                       let () = diCB := None in cb ()) in
          ((match (oldSub, newSub) with
            | (NoSub, NoSub) -> newSub
            | (((Registration
               (oldKey, _oldEnCB, oldDiCB))[@implicit_arity ]),
               ((Registration
               (newKey, _newEnCB, newDiCB))[@implicit_arity ])) when
                oldKey = newKey -> let () = newDiCB := (!oldDiCB) in newSub
            | (((Mapper (oldMapper, oldSubSub))[@implicit_arity ]), ((Mapper
               (newMapper, newSubSub))[@implicit_arity ])) ->
                let olderCallbacks = oldMapper oldCallbacks in
                let newerCallbacks = newMapper newCallbacks in
                let _newerSubSub =
                  run olderCallbacks newerCallbacks oldSubSub newSubSub in
                newSub
            | (((Batch (oldSubs))[@explicit_arity ]), ((Batch
               (newSubs))[@explicit_arity ])) ->
                let rec aux oldList newList =
                  match (oldList, newList) with
                  | ([], []) -> ()
                  | ([], newSubSub::newRest) ->
                      let () = enable newCallbacks newSubSub in
                      aux [] newRest
                  | (oldSubSub::oldRest, []) ->
                      let () = disable oldCallbacks oldSubSub in
                      aux oldRest []
                  | (oldSubSub::oldRest, newSubSub::newRest) ->
                      let _newerSubSub =
                        run oldCallbacks newCallbacks oldSubSub newSubSub in
                      aux oldRest newRest in
                let () = aux oldSubs newSubs in newSub
            | (oldS, newS) ->
                let () = disable oldCallbacks oldS in
                let () = enable newCallbacks newS in newSub)
            [@ocaml.warning "-4"])