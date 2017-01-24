
(* https://github.com/Matt-Esch/virtual-dom/blob/master/docs/vnode.md *)



type 'msg applicationCallbacks = {
  enqueue : 'msg -> unit;
}


(* Attributes are not properties *)
(* https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes *)

type 'msg property =
  | NoProp
  | RawProp of string * string (* TODO:  This last string needs to be made something more generic, maybe a function... *)
  (* Attribute is (namespace, key, value) *)
  | Attribute of string * string * string
  | Data of string * string
  (* Event is (type, userkey, callback) *)
  | Event of string * string * (Web.Node.event -> 'msg option)
  (* | Event of string * (Web.Event.t -> 'msg) *)
  | Style of (string * string) list

type 'msg properties = 'msg property list

(* type 'msg taggerCallbacks =
  { renderToHtmlString : unit -> string
  ; patchVNodesIntoElement : 'msg applicationCallbacks ref -> Web.Node.t
  } *)

type 'msg t =
  | CommentNode of string
  | Text of string
  (* Node (namespace, tagName, key, unique, properties, children)  *)
  | Node of string * string * string * string * 'msg properties * 'msg t list
  (* | ArrayNode of string * string * string * string * 'msg property array * 'msg t array *)
  (* LazyGen (key, fnGenerator) *)
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  (* Tagger (toString, toDom, toVNodes) *)
(* | Tagger of (unit -> string) * ('msg applicationCallbacks ref -> Web.Node.t -> Web.Node.t -> int ->  'msg t list -> Web.Node.t) * (unit -> 'msg t) *)
  (* Tagger (tagger, vdom) *)
  | Tagger of ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t
  (*  *)
  (* | Tagger of (('a -> 'msg) -> 'a t -> 'msg t) *)
  (* Custom (key, cbAdd, cbRemove, cbChange, properties, children) *)
  (* | Custom of string * (unit -> Web.Node.t) * (Web.Node.t -> unit) * *)



(* Nodes *)

let noNode = CommentNode ""

let comment s = CommentNode s

let text s = Text s

let fullnode namespace tagName key unique props vdoms =
  Node (namespace, tagName, key, unique, props, vdoms)

let node ?(namespace="") tagName ?(key="") ?(unique="") props vdoms =
  fullnode namespace tagName key unique props vdoms

(* let arraynode namespace tagName key unique props vdoms =
  ArrayNode (namespace, tagName, key, unique, props, vdoms) *)

let lazyGen key fn =
  LazyGen (key, fn, ref noNode)

(* Properties *)

let noProp = NoProp

let prop key value = RawProp (key, value)

(* `on` sets no key, so it will not be updated on the DOM unless its position changes *)
(* let on name cb = Event (name, "", cb) *)
(* let on name ?(key="") cb = Event (name, key, cb) *)
(* let on name cb = Event (name, cb) *)
let on name key cb = Event (name, key, cb)

let attribute namespace key value = Attribute (namespace, key, value)


let data key value = Data (key, value)

let style key value = Style [ (key, value) ]

let styles s = Style s

(* Accessors *)

(* Inefficient, but purely for debugging *)
let rec renderToHtmlString = function
  | CommentNode s -> "<!-- " ^ s ^ " -->"
  | Text s -> s
  | Node (namespace, tagName, _key, _unique, props, vdoms) ->
    let renderProp = function
      | NoProp -> ""
      | RawProp (k, v) -> String.concat "" [" "; k; "=\""; v; "\""]
      | Attribute (_namespace, k, v) -> String.concat "" [" "; k; "=\""; v; "\""]
      | Data (k, v) -> String.concat "" [" data-"; k; "=\""; v; "\""]
      | Event (typ, _key, v) -> String.concat "" [" "; typ; "=\"js:"; Js.typeof v; "\""]
      | Style s -> String.concat "" [" style=\""; String.concat ";" (List.map (fun (k, v) -> String.concat "" [k;":";v;";"]) s); "\""]
    in
    String.concat ""
      [ "<"
      ; namespace
      ; if namespace = "" then "" else ":"
      ; tagName
      ; String.concat "" (List.map (fun p -> renderProp p) props)
      ; ">"
      ; String.concat "" (List.map (fun v -> renderToHtmlString v) vdoms)
      ; "</"
      ; tagName
      ; ">"
      ]
  | LazyGen (_key, gen, _cache) ->
    let vdom = gen () in
    renderToHtmlString vdom
  | Tagger (_tagger, vdom) -> renderToHtmlString vdom

  (* | KeyedNode (elemType, props, vdoms) -> String.concat ":" ["UNIMPLEMENTED"; elemType] *)


(* Patch elements *)



(* let applyProperties callbacks elem curProperties =
  List.fold_left
    (fun elem -> function
       | NoProp -> elem
       | RawProp (k, v) -> elem
       | Attribute (namespace, k, v) -> elem
       | Data (k, v) -> elem
       | Event (typ, _key, v) ->
         (* let () = Js.log [|"Event:"; typ|] in *)
         let cb : Web.Node.event_cb =
           fun [@bs] ev ->
             match v ev with
             | None -> ()
             | Some msg -> !callbacks.enqueue msg in
         let () = Web_node.addEventListener elem typ cb false in
         elem
       | Style s -> List.fold_left (fun elem (k, v) -> let () = Web.Node.setStyleProperty elem k (Js.Null.return v) in elem) elem s
       (* | Style s -> List.fold_left (fun (k, v) elem -> let _ = elem##style##set k v in elem) elem s *)
    ) elem curProperties


(* Creating actual DOM elements *)
(* let doc = Web.document *)

let createElementFromVNode_addProps callbacks properties elem =
  applyProperties callbacks elem properties


let rec createElementFromVNode_addChildren callbacks children elem =
  children |> List.fold_left (fun n child -> let _childelem = Web.Node.appendChild n (createElementFromVNode callbacks child) in n) elem
    and createElementFromVNode callbacks = function
  | CommentNode s -> Web.Document.createComment s
  | Text text -> Web.Document.createTextNode text
  | Node (namespace, _key_unused, tagName, properties, children) -> (* let () = Js.log (callbacks, namespace, _key_unused, tagName, properties, children) in *)
    let child = Web.Document.createElementNsOptional namespace tagName in
    (* let () = Js.log ("Blooop", child) in *)
    child
    |> createElementFromVNode_addProps callbacks properties
    |> createElementFromVNode_addChildren callbacks children
    (* Web.Document.createElementNsOptional namespace tagName
    |> createElementFromVNode_addProps callbacks properties
    |> createElementFromVNode_addChildren callbacks children *)


let createVNodesIntoElement callbacks vnodes elem =
  vnodes |> List.fold_left (fun n vnode -> let _childelem = Web.Node.appendChild n (createElementFromVNode callbacks vnode) in n) elem

let createVNodeIntoElement callbacks vnode elem =
  createVNodesIntoElement callbacks [vnode] elem *)



(* Diffing/Patching *)
(* Very naive right now, but it worksish *)

(* let rec patchVNodesOnElems_DeleteRest elem elems idx =
  if Js.Array.length elems >= idx then -1 else
    let child = elems.(idx) in
    let _removedChild = Web.Node.removeChild elem child in
    patchVNodesOnElems_DeleteRest elem elems idx *)

(* let rec patchVNodesOnElems_Empty callbacks elem elems idx = function
  | [] -> -1
  | NoVNode s :: newVNodes ->
    let child = Web.Document.createComment s in
    let _newChild = Web.Node.appendChild elem child in
    patchVNodesOnElems_Empty callbacks elem elems (idx+1) newVNodes
  | Text text :: newVNodes ->
    let child = Web.Document.createTextNode text in
    let _newChild = Web.Node.appendChild elem child in
    patchVNodesOnElems_Empty callbacks elem elems (idx+1) newVNodes
  | vnode :: newVNodes ->
    let child = createElementFromVNode callbacks vnode in
    let _newChild = Web.Node.appendChild elem child in
    patchVNodesOnElems_Empty callbacks elem elems (idx+1) newVNodes
    (* let child = elems.(idx) in *)
    (* let _child = Web.Node.removeChild elem child in *)
    (* patchVNodesOnElems_Empty callbacks elem elems idx newVNodes (* Not incrementing idx since we just removed something *) *) *)


let _handlerName idx typ =
  "_handler_" ^ (string_of_int idx) ^ typ

let _usercb callbacks f =
  fun ev ->
    match f ev with
    | None -> () (*Js.log "Nothing"*)
    | Some msg -> (*let () = Js.log ("Handling msg", msg) in*) !callbacks.enqueue msg

let patchVNodesOnElems_PropertiesApply_Add callbacks elem idx = function
  | NoProp -> ()
  | RawProp (k, v) -> Web.Node.setProp elem k v
  | Attribute (namespace, k, v) -> Web.Node.setAttributeNsOptional elem namespace k v
  | Data (k, v) -> Js.log ("TODO:  Add Data Unhandled", k, v); failwith "TODO:  Add Data Unhandled"
  | Event (t, _k, f) ->
    (* let () = Js.log ("Adding event", elem, t, k, f) in *)
    (* let cb : Web.Node.event_cb =
      fun [@bs] ev ->
        (* let () = Js.log ("ON-EVENT", elem, idx, ev) in *)
        match f ev with
        | None -> () (*Js.log "Nothing"*)
        | Some msg -> (*let () = Js.log ("Handling msg", msg) in*) !callbacks.enqueue msg in
        (* let msg = f ev in
           !callbacks.enqueue msg in *) *)
    let cb = _usercb callbacks f in
    let handler : Web.Node.event_cb =
      fun [@bs] ev -> match Js.Undefined.to_opt ev##target with
        | None -> failwith "Element Event called without being attached to an element?!  Report this with minimal test case!"
        | Some _target ->
          (* let () = Js.log ("ON-EVENT", elem, idx, ev) in *)
          let userCB = Web.Node.getProp elem (_handlerName idx "cb") in
          userCB ev [@bs] in
    let () = Web.Node.setProp elem (_handlerName idx "cb") (Js.Undefined.return cb) in
    let () = Web.Node.setProp_asEventListener elem (_handlerName idx "") (Js.Undefined.return handler) in
    Web.Node.addEventListener elem t handler false
  | Style s ->
    List.fold_left (fun () (k, v) -> Web.Node.setStyleProperty elem k (Js.Null.return v)) () s

let patchVNodesOnElems_PropertiesApply_Remove _callbacks elem idx = function
  | NoProp -> ()
  | RawProp (k, _v) -> Web.Node.setProp elem k Js.Undefined.empty
  | Attribute (namespace, k, _v) -> Web.Node.removeAttributeNsOptional elem namespace k
  | Data (k, v) -> Js.log ("TODO:  Remove Data Unhandled", k, v); failwith "TODO:  Remove Data Unhandled"
  | Event (t, _k, _f) ->
    (* let () = Js.log ("Removing Event", elem, t, k, f) in *)
    let () = match Js.Undefined.to_opt (Web.Node.getProp_asEventListener elem (_handlerName idx)) with
      | None -> failwith "Something else has messed with the DOM, inconsistent state!"
      | Some cb -> Web.Node.removeEventListener elem t cb false in
    let () = Web.Node.setProp elem (_handlerName idx "cb") Js.Undefined.empty in
    let () = Web.Node.setProp_asEventListener elem (_handlerName idx "") Js.Undefined.empty in
    ()
  | Style s -> List.fold_left (fun () (k, _v) -> Web.Node.setStyleProperty elem k Js.Null.empty) () s

let patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp newProp =
  let () = patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp in
  let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
  ()

let patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp = function
  | NoProp as _newProp -> failwith "This should never be called as all entries through NoProp are gated."
  | RawProp (k, v) as _newProp ->
    (* let () = Js.log ("Mutating RawProp", elem, oldProp, _newProp) in *)
    Web.Node.setProp elem k v (* Wow setting properties is slow, unsure how to optimize this further though... *)
  | Attribute (namespace, k, v) as _newProp ->
    (* let () = Js.log ("Mutating Attribute", namespace, k, v, elem) in *)
    Web.Node.setAttributeNsOptional elem namespace k v
  | Data  (k, v) as _newProp -> Js.log ("TODO:  Mutate Data Unhandled", k, v)
  (* Wow but adding and removing event handlers is slow on the DOM, lets do this to see if it is faster... *)
  (* Initial profiling tests reveal a fairly substantial speed improvement in event handlers switching now! *)
  | Event (t, _k, f) as newProp ->
    (* let () = Js.log ("Mutating event", elem, oldProp, newProp) in *)
    let oldT = match [@ocaml.warning "-4"] oldProp with
      | Event (oldT, _oldK, _oldF) -> oldT
      | _ -> failwith "This should never be called as all entries to mutate are gated to the same types" in
    if oldT = t then
      let cb = _usercb callbacks f in
      let () = Web.Node.setProp elem (_handlerName idx "cb") (Js.Undefined.return cb) in
      ()
    else patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp newProp
  | Style s as _newProp ->
    (* let () = Js.log ("Mutating Style", elem, oldProp, _newProp) in *)
    match [@ocaml.warning "-4"] oldProp with
    | Style oldS ->
      List.fold_left2 (fun () (ok, ov) (nk, nv) ->
          if ok = nk then
            if ov = nv then
              ()
            else
              Web.Node.setStyleProperty elem nk (Js.Null.return nv)
          else
            let () = Web.Node.setStyleProperty elem ok Js.Null.empty in
            Web.Node.setStyleProperty elem nk (Js.Null.return nv)
        ) () oldS s
    | _ -> failwith "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"

let rec patchVNodesOnElems_PropertiesApply callbacks elem idx oldProperties newProperties =
  (* let () = Js.log ("PROPERTY-APPLY", elem, idx, oldProperties, newProperties) in *)
  match [@ocaml.warning "-4"] oldProperties, newProperties with
  | [], [] -> ()
  | [], newProp :: newRest ->
    let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) [] newRest
  | oldProp :: oldRest, [] ->
    let () = patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) [] oldRest
  (* NoProp *)
  | NoProp :: oldRest, NoProp :: newRest -> patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* RawProp *)
  | (RawProp (oldK, oldV) as oldProp) :: oldRest, (RawProp (newK, newV) as newProp) :: newRest ->
    (* let () = Js.log ("RawProp Test", elem, idx, oldProp, newProp, oldK = newK && oldV = newV, oldRest, newRest) in *)
    let () = if oldK = newK && oldV = newV then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Attribute *)
  | (Attribute (oldNS, oldK, oldV) as oldProp) :: oldRest, (Attribute (newNS, newK, newV) as newProp) :: newRest ->
    let () = if oldNS = newNS && oldK = newK && oldV = newV then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Data *)
  | (Data (oldK, oldV) as oldProp) :: oldRest, (Data (newK, newV) as newProp) :: newRest ->
    let () = if oldK = newK && oldV = newV then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Event *)
  (* | Event (oldTyp, oldKey, oldCbev) :: oldRest, Event (newTyp, newKey, newCbev) :: newRest ->
     let () = if oldTyp = newTyp && oldKey = newKey then () else *)
  | (Event (oldTyp, oldKey, _oldCbev) as oldProp) :: oldRest, (Event (newTyp, newKey, _newCbev) as newProp) :: newRest ->
    (* let () = Js.log ("Event Test", elem, idx, oldProp, newProp, oldTyp = newTyp && oldKey = newKey, oldRest, newRest) in *)
    (* TODO:  This is such a *BAD* way of doing this, but event removal needs to be passed in the same func as what was
       registered.  So we enforce a string key, which is more than what some virtualdoms allow for at least since with
       a key you can have multiple events of the same type registered without issue... *)
    let () = if oldTyp = newTyp && oldKey = newKey then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Style *)
  | (Style oldS as oldProp) :: oldRest, (Style newS as newProp) :: newRest ->
    let () = if oldS = newS then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  | oldProp :: oldRest, newProp :: newRest ->
    let () = patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest


let patchVNodesOnElems_Properties callbacks elem oldProperties newProperties =
  (* Profiling here show `=` to be very slow, but testing reveals it to be faster than checking through the properties
     manually on times when there are few to no changes, which is most of the time, so keeping it for now... *)
  (* TODO:  Look into if there is a better way to quick test property comparisons, especially since it likely returns
     false when events are included regardless of anything else. *)
  (* if oldProperties = newProperties then
    ()
  else *)
    patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties newProperties

     (* | NoProp -> elem
     | RawProp (k, v) -> elem
     | Attribute (namespace, k, v) -> elem
     | Data (k, v) -> elem
     | Event (typ, v) ->
       (* let () = Js.log [|"Event:"; typ|] in *)
       let cb : Web.Event.cb =
         fun [@bs] ev ->
           let msg = v ev in
           !callbacks.enqueue msg in
       let () = Web_node.addEventListener elem typ cb false in
       elem
     | Style s -> List.fold_left (fun elem (k, v) -> let () = Web.Node.setStyleProperty elem k v in elem) elem s *)


(* let patchVNodesOnElems_ReplaceVnodeAt callbacks elem elems idx vnode =
  let child = elems.(idx) in
  let _removedChild = Web.Node.removeChild elem child in
  let newChild = createElementFromVNode callbacks vnode in
  let _attachedChild = Web.Node.appendChild elem newChild in
  () *)

(* let patchVNodesOnElems_createElement callbacks namespace tagName properties =
    let child = Web.Document.createElementNsOptional namespace tagName in
    let () = patchVNodesOnElems_Properties callbacks child [] properties in
    child *)


let rec patchVNodesOnElems_ReplaceNode callbacks elem elems idx = function [@ocaml.warning "-4"]
  | (Node (newNamespace, newTagName, _newKey, _newUnique, newProperties, newChildren)) ->
    let oldChild = elems.(idx) in
    let newChild = Web.Document.createElementNsOptional newNamespace newTagName in
    let () = patchVNodesOnElems_Properties callbacks newChild [] newProperties in
    let childChildren = Web.Node.childNodes newChild in
    let () = patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren in
    let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
    let _removedChild = Web.Node.removeChild elem oldChild in
    (* let () = Js.log ("Fullswap happened", oldChild, newChild) in *)
    ()
  | _ -> failwith "Node replacement should never be passed anything but a node itself"

and patchVNodesOnElems_CreateElement callbacks = function
  | CommentNode s -> Web.Document.createComment s
  | Text text -> Web.Document.createTextNode text
  | Node (newNamespace, newTagName, _newKey, _unique, newProperties, newChildren) ->
    let newChild = Web.Document.createElementNsOptional newNamespace newTagName in
    let () = patchVNodesOnElems_Properties callbacks newChild [] newProperties in
    let childChildren = Web.Node.childNodes newChild in
    let () = patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren in
    newChild
  | LazyGen (_newKey, newGen, newCache) ->
    let vdom = newGen () in
    let () = newCache := vdom in
    patchVNodesOnElems_CreateElement callbacks vdom
  | Tagger (tagger, vdom) ->
    (* let () = Js.log ("Tagger", "creating", tagger, vdom) in *)
    patchVNodesOnElems_CreateElement (tagger callbacks) vdom

and patchVNodesOnElems callbacks elem elems idx oldVNodes newVNodes =
  (* let () = Js.log ("patchVNodesOnElems", elem, elems, idx, oldVNodes, newVNodes) in *)
  match [@ocaml.warning "-4"] oldVNodes, newVNodes with
  | Tagger (_oldTagger, oldVdom) :: oldRest, _ ->
    (* let () = Js.log ("Tagger", "old", oldTagger, oldVdom) in *)
    patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) newVNodes
  | oldNode :: oldRest, Tagger (newTagger, newVdom) :: newRest ->
    (* let () = Js.log ("Tagger", "new", newTagger, newVdom) in *)
    let () = patchVNodesOnElems (newTagger callbacks) elem elems idx [oldNode] [newVdom] in
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | [], [] -> ()
  | [], newNode :: newRest ->
    let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
    let _attachedChild = Web.Node.appendChild elem newChild in
    patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
  | _oldVnode :: oldRest, [] ->
    let child = elems.(idx) in
    let _removedChild = Web.Node.removeChild elem child in
    patchVNodesOnElems callbacks elem elems idx oldRest [] (* Not changing idx so we can delete the rest too *)
  | CommentNode oldS :: oldRest, CommentNode newS :: newRest when oldS = newS -> patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
  | Text oldText :: oldRest, Text newText :: newRest ->
    let () = if oldText = newText then () else
      let child = elems.(idx) in
      Web.Node.set_nodeValue child newText in
    patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
  | LazyGen (oldKey, _oldGen, oldCache) :: oldRest, LazyGen (newKey, newGen, newCache) :: newRest ->
    if oldKey = newKey then
      (* let () = Js.log ("Lazy match!", oldKey, newKey, elem, elems, idx) in *)
      let () = newCache := !oldCache in (* Don't forget to pass the cache along... *)
      patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
    else
      ( match oldRest, newRest with
        | LazyGen (olderKey, _olderGen, _olderCache) :: olderRest,
          LazyGen (newerKey, _newerGen, _newerCache) :: newerRest when olderKey = newKey && oldKey = newerKey ->
          (* let () = Js.log ("Lazy older newer swap", olderKey, oldKey, newKey, newerKey, elem, elems.(idx)) in *)
          (* TODO:  Test this branch, it is untested thus far *)
          let firstChild = elems.(idx) in
          let secondChild = elems.(idx+1) in
          let _removedChild = Web.Node.removeChild elem secondChild in
          let _attachedChild = Web.Node.insertBefore elem secondChild firstChild in
          patchVNodesOnElems callbacks elem elems (idx+2) olderRest newerRest
        | LazyGen (olderKey, _olderGen, olderCache) :: olderRest, _ when olderKey = newKey ->
          (* let () = Js.log ("Lazy older match", olderKey, oldKey, newKey, elem, elems.(idx)) in *)
          let oldChild = elems.(idx) in
          let _removedChild = Web.Node.removeChild elem oldChild in
          let oldVdom = !olderCache in
          let () = newCache := oldVdom in (* Don't forget to pass the cache along... *)
          patchVNodesOnElems callbacks elem elems (idx+1) olderRest newRest
        | _, LazyGen (newerKey, _newerGen, _newerCache) :: _newerRest when newerKey = oldKey ->
          (* let () = Js.log ("Lazy newer match", "parse", oldKey, newKey, newerKey, elem, elems.(idx)) in *)
          let oldChild = elems.(idx) in
          let newVdom = newGen () in
          let () = newCache := newVdom in (* Don't forget to pass the cache along... *)
          let newChild = patchVNodesOnElems_CreateElement callbacks newVdom in
          let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
          patchVNodesOnElems callbacks elem elems (idx+1) oldVNodes newRest
        | _ ->
          (* let () = Js.log ("Lazy nomatch", oldKey, newKey, elem, elems.(idx)) in *)
          let oldVdom = !oldCache in
          let newVdom = newGen () in
          let () = newCache := newVdom in (* Don't forget to pass the cache along... *)
          patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) (newVdom :: newRest)
      )
  | Node (oldNamespace, oldTagName, oldKey, oldUnique, oldProperties, oldChildren) :: oldRest,
    (Node (newNamespace, newTagName, newKey, newUnique, newProperties, newChildren) as newNode) :: newRest ->
    if newKey = "" || oldKey = "" then
      if oldUnique = newUnique then
        (* let () = Js.log ("Node test", "parse", elem, elems.(idx), newNode) in *)
        let child = elems.(idx) in
        let childChildren = Web.Node.childNodes child in
        let () = patchVNodesOnElems_Properties callbacks child oldProperties newProperties in
        let () = patchVNodesOnElems callbacks child childChildren 0 oldChildren newChildren in
        patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
      else
        (* let () = Js.log ("Node test", "unique swap", elem, elems.(idx), newNode) in *)
        let () = patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode in
        patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
    else if oldKey = newKey then
      (* let () = Js.log ("Node test", "match", elem, elems.(idx), newNode) in *)
      patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
    else (* Keys do not match but do exist *)
      ( match oldRest, newRest with
        | Node (olderNamespace, olderTagName, olderKey, _olderUnique, _olderProperties, _olderChildren) :: olderRest,
          Node (newerNamespace, newerTagName, newerKey, _newerUnique, _newerProperties, _newerChildren) :: newerRest
          when olderNamespace = newNamespace && olderTagName = newTagName && olderKey = newKey &&
               oldNamespace = newerNamespace && oldTagName = newerTagName && oldKey = newerKey ->
          (* let () = Js.log ("Node test", "older newer swap", elem, elems.(idx), newNode) in *)
          (* TODO:  Test this branch, it is untested thus far *)
          let firstChild = elems.(idx) in
          let secondChild = elems.(idx+1) in
          let _removedChild = Web.Node.removeChild elem secondChild in
          let _attachedChild = Web.Node.insertBefore elem secondChild firstChild in
          patchVNodesOnElems callbacks elem elems (idx+2) olderRest newerRest
        | Node (olderNamespace, olderTagName, olderKey, _olderUnique, _olderProperties, _olderChildren) :: olderRest, _
          when olderNamespace = newNamespace && olderTagName = newTagName && olderKey = newKey ->
          (* let () = Js.log ("Node test", "older match", elem, elems.(idx), newNode) in *)
          let oldChild = elems.(idx) in
          let _removedChild = Web.Node.removeChild elem oldChild in
          patchVNodesOnElems callbacks elem elems (idx+1) olderRest newRest
        | _, Node (newerNamespace, newerTagName, newerKey, _newerUnique, _newerProperties, _newerChildren) :: _newerRest
          when oldNamespace = newerNamespace && oldTagName = newerTagName && oldKey = newerKey ->
            (* let () = Js.log ("Node test", "newer match", elem, elems.(idx), newNode) in *)
          let oldChild = elems.(idx) in
          let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
          let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
          patchVNodesOnElems callbacks elem elems (idx+1) oldVNodes newRest
        | _ ->
          if oldUnique = newUnique then
            (* let () = Js.log ("Node test", "non-older non-newer keyed parse", elem, elems.(idx), newNode) in *)
            let child = elems.(idx) in
            let childChildren = Web.Node.childNodes child in
            let () = patchVNodesOnElems_Properties callbacks child oldProperties newProperties in
            let () = patchVNodesOnElems callbacks child childChildren 0 oldChildren newChildren in
            patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
          else
            (* let () = Js.log ("Node test", "non-older keyed unique swap", elem, elems.(idx), newNode) in *)
            let () = patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode in
            patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
      )
  | _oldVnode :: oldRest, newNode :: newRest ->
    let oldChild = elems.(idx) in
    let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
    let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
    let _removedChild = Web.Node.removeChild elem oldChild in
    patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest



let patchVNodesIntoElement callbacks elem oldVNodes newVNodes =
  let elems = Web.Node.childNodes elem in
  let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in (* Planning to return an altered vdom set here instead of using mutation... *)
  newVNodes

let patchVNodeIntoElement callbacks elem oldVNode newVNode =
  patchVNodesIntoElement callbacks elem [oldVNode] [newVNode]


(* Node namespace key tagName properties children  *)
(* | Node of string option * string option * string * 'msg property list * 'msg velem list *)



let wrapCallbacks func callbacks =
  ref
    { enqueue = (fun msg -> !callbacks.enqueue (func msg))
    }

let map : ('a -> 'b) -> 'a t -> 'b t = fun func vdom ->
  let tagger callbacks =
    ref
      { enqueue = (fun msg -> !callbacks.enqueue (func msg))
      } in
  Tagger (Obj.magic tagger, Obj.magic vdom)

(* let map func vdom =
  let toString () = renderToHtmlString vdom in
  let toDom in
  Tagger (toString, toDom, toVNodes) *)
