type 'msg systemMessage =
  | Render 
  | AddRenderMsg of 'msg 
  | RemoveRenderMsg of 'msg 
type 'msg applicationCallbacks =
  {
  enqueue: 'msg -> unit ;
  on: 'msg systemMessage -> unit }
type 'msg eventHandler =
  | EventHandlerCallback of string * (Web.Node.event -> 'msg option) 
  | EventHandlerMsg of 'msg 
type 'msg eventCache =
  {
  handler: Web.Node.event_cb ;
  cb: (Web.Node.event -> 'msg option) ref }
type 'msg property =
  | NoProp 
  | RawProp of string * string 
  | Attribute of string * string * string 
  | Data of string * string 
  | Event of string * 'msg eventHandler * 'msg eventCache option ref 
  | Style of (string * string) list 
type 'msg properties = 'msg property list
type 'msg t =
  | CommentNode of string 
  | Text of string 
  | Node of string * string * string * string * 'msg properties * 'msg t list
  
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref 
  | Tagger of
  ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t 
let noNode = ((((CommentNode (""))[@explicit_arity ]) : 'msg t) : 'msg t)
let comment (s : string) = (((CommentNode (s))[@explicit_arity ]) : 'msg t)
let text (s : string) = (((Text (s))[@explicit_arity ]) : 'msg t)
let fullnode (namespace : string) (tagName : string) (key : string)
  (unique : string) (props : 'msg properties) (vdoms : 'msg t list) =
  (((Node (namespace, tagName, key, unique, props, vdoms))
  [@implicit_arity ]) : 'msg t)
let node ?namespace:((namespace : string)= "")  (tagName : string)
  ?key:((key : string)= "")  ?unique:((unique : string)= "") 
  (props : 'msg properties) (vdoms : 'msg t list) =
  (fullnode namespace tagName key unique props vdoms : 'msg t)
let lazyGen (key : string) (fn : unit -> 'msg t) =
  (((LazyGen (key, fn, (ref noNode)))[@implicit_arity ]) : 'msg t)
let noProp = ((NoProp : 'msg property) : 'msg property)
let prop (key : string) (value : string) = (((RawProp (key, value))
  [@implicit_arity ]) : 'msg property)
let onCB (name : string) (key : string) (cb : Web.Node.event -> 'msg option)
  =
  (((Event
       (name, ((EventHandlerCallback (key, cb))[@implicit_arity ]),
         (ref None)))
  [@implicit_arity ]) : 'msg property)
let onMsg (name : string) (msg : 'msg) =
  (((Event (name, ((EventHandlerMsg (msg))[@explicit_arity ]), (ref None)))
  [@implicit_arity ]) : 'msg property)
let attribute (namespace : string) (key : string) (value : string) =
  (((Attribute (namespace, key, value))[@implicit_arity ]) : 'msg property)
let data (key : string) (value : string) = (((Data (key, value))
  [@implicit_arity ]) : 'msg property)
let style (key : string) (value : string) = (((Style ([(key, value)]))
  [@explicit_arity ]) : 'msg property)
let styles s = (((Style (s))[@explicit_arity ]) : 'msg property)
let rec renderToHtmlString =
  ((function
    | ((CommentNode (s))[@explicit_arity ]) -> "<!-- " ^ (s ^ " -->")
    | ((Text (s))[@explicit_arity ]) -> s
    | ((Node
        (namespace, tagName, _key, _unique, props, vdoms))[@implicit_arity ])
        ->
        let renderProp =
          function
          | NoProp -> ""
          | ((RawProp (k, v))[@implicit_arity ]) ->
              String.concat "" [" "; k; "=\""; v; "\""]
          | ((Attribute (_namespace, k, v))[@implicit_arity ]) ->
              String.concat "" [" "; k; "=\""; v; "\""]
          | ((Data (k, v))[@implicit_arity ]) ->
              String.concat "" [" data-"; k; "=\""; v; "\""]
          | ((Event (_, _, _))[@implicit_arity ]) -> ""
          | ((Style (s))[@explicit_arity ]) ->
              String.concat ""
                [" style=\"";
                String.concat ";"
                  (List.map (fun (k, v) -> String.concat "" [k; ":"; v; ";"])
                     s);
                "\""] in
        String.concat ""
          ["<";
          namespace;
          if namespace = "" then "" else ":";
          tagName;
          String.concat "" (List.map (fun p -> renderProp p) props);
          ">";
          String.concat "" (List.map (fun v -> renderToHtmlString v) vdoms);
          "</";
          tagName;
          ">"]
    | ((LazyGen (_key, gen, _cache))[@implicit_arity ]) ->
        let vdom = gen () in renderToHtmlString vdom
    | ((Tagger (_tagger, vdom))[@implicit_arity ]) -> renderToHtmlString vdom : 
  'msg t -> string) : 'msg t -> string)
let emptyEventHandler = ((((fun _ev -> ())
  [@bs ]) : Web.Node.event_cb) : Web.Node.event_cb)
let emptyEventCB _ev = (None : Web.Node.event_cb option)
let eventHandler (callbacks : 'msg applicationCallbacks ref)
  (cb : (Web.Node.event -> 'msg option) ref) =
  (((fun ev ->
       match (!cb) ev with
       | None -> ()
       | ((Some (msg))[@explicit_arity ]) -> (!callbacks).enqueue msg)
  [@bs ]) : Web.Node.event_cb)
let eventHandler_GetCB =
  ((function
    | ((EventHandlerCallback (_, cb))[@implicit_arity ]) -> cb
    | ((EventHandlerMsg (msg))[@explicit_arity ]) ->
        (fun _ev -> ((Some (msg))[@explicit_arity ])) : 'msg eventHandler ->
                                                          Web.Node.event ->
                                                            'msg option) : 
  'msg eventHandler -> Web.Node.event -> 'msg option)
let compareEventHandlerTypes (left : 'msg eventHandler) =
  (function
   | ((EventHandlerCallback (cb, _))[@implicit_arity ]) ->
       (match left with
        | ((EventHandlerCallback (lcb, _))[@implicit_arity ]) when cb = lcb
            -> true
        | _ -> false)
   | ((EventHandlerMsg (msg))[@explicit_arity ]) ->
       (match left with
        | ((EventHandlerMsg (lmsg))[@explicit_arity ]) when msg = lmsg ->
            true
        | _ -> false) : 'msg eventHandler -> bool)
let eventHandler_Register (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (name : string) (handlerType : 'msg eventHandler) =
  (let cb = ref (eventHandler_GetCB handlerType) in
   let handler = eventHandler callbacks cb in
   let () = Web.Node.addEventListener elem name handler false in
   ((Some ({ handler; cb }))[@explicit_arity ]) : 'msg eventCache option)
let eventHandler_Unregister (elem : Web.Node.t) (name : string) =
  (function
   | None -> None
   | ((Some (cache))[@explicit_arity ]) ->
       let () = Web.Node.removeEventListener elem name cache.handler false in
       None : 'msg eventCache option -> 'msg eventCache option)
let eventHandler_Mutate (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (oldName : string) (newName : string)
  (oldHandlerType : 'msg eventHandler) (newHandlerType : 'msg eventHandler)
  (oldCache : 'msg eventCache option ref)
  (newCache : 'msg eventCache option ref) =
  (match !oldCache with
   | None ->
       newCache :=
         (eventHandler_Register callbacks elem newName newHandlerType)
   | ((Some (oldcache))[@explicit_arity ]) ->
       if oldName = newName
       then
         let () = newCache := (!oldCache) in
         (if compareEventHandlerTypes oldHandlerType newHandlerType
          then ()
          else
            (let cb = eventHandler_GetCB newHandlerType in
             let () = oldcache.cb := cb in ()))
       else
         (let () =
            oldCache := (eventHandler_Unregister elem oldName (!oldCache)) in
          let () =
            newCache :=
              (eventHandler_Register callbacks elem newName newHandlerType) in
          ()) : unit)
let patchVNodesOnElems_PropertiesApply_Add
  (callbacks : 'msg applicationCallbacks ref) (elem : Web.Node.t)
  (_idx : int) =
  (function
   | NoProp -> ()
   | ((RawProp (k, v))[@implicit_arity ]) -> Web.Node.setProp elem k v
   | ((Attribute (namespace, k, v))[@implicit_arity ]) ->
       Web.Node.setAttributeNsOptional elem namespace k v
   | ((Data (k, v))[@implicit_arity ]) ->
       (Js.log ("TODO:  Add Data Unhandled", k, v);
        failwith "TODO:  Add Data Unhandled")
   | ((Event (name, handlerType, cache))[@implicit_arity ]) ->
       cache := (eventHandler_Register callbacks elem name handlerType)
   | ((Style (s))[@explicit_arity ]) ->
       List.fold_left
         (fun () ->
            fun (k, v) -> Web.Node.setStyleProperty elem k (Js.Null.return v))
         () s : 'msg property -> unit)
let patchVNodesOnElems_PropertiesApply_Remove
  (_callbacks : 'msg applicationCallbacks ref) (elem : Web.Node.t)
  (_idx : int) =
  (function
   | NoProp -> ()
   | ((RawProp (k, _v))[@implicit_arity ]) ->
       Web.Node.setProp elem k Js.Undefined.empty
   | ((Attribute (namespace, k, _v))[@implicit_arity ]) ->
       Web.Node.removeAttributeNsOptional elem namespace k
   | ((Data (k, v))[@implicit_arity ]) ->
       (Js.log ("TODO:  Remove Data Unhandled", k, v);
        failwith "TODO:  Remove Data Unhandled")
   | ((Event (name, _, cache))[@implicit_arity ]) ->
       cache := (eventHandler_Unregister elem name (!cache))
   | ((Style (s))[@explicit_arity ]) ->
       List.fold_left
         (fun () ->
            fun (k, _v) -> Web.Node.setStyleProperty elem k Js.Null.empty) ()
         s : 'msg property -> unit)
let patchVNodesOnElems_PropertiesApply_RemoveAdd
  (callbacks : 'msg applicationCallbacks ref) (elem : Web.Node.t) (idx : int)
  (oldProp : 'msg property) (newProp : 'msg property) =
  (let () =
     patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp in
   let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
   () : unit)
let patchVNodesOnElems_PropertiesApply_Mutate
  (_callbacks : 'msg applicationCallbacks ref) (elem : Web.Node.t)
  (_idx : int) (oldProp : 'msg property) =
  (function
   | NoProp as _newProp ->
       failwith
         "This should never be called as all entries through NoProp are gated."
   | ((RawProp (k, v))[@implicit_arity ]) as _newProp ->
       Web.Node.setProp elem k v
   | ((Attribute (namespace, k, v))[@implicit_arity ]) as _newProp ->
       Web.Node.setAttributeNsOptional elem namespace k v
   | ((Data (k, v))[@implicit_arity ]) as _newProp ->
       (Js.log ("TODO:  Mutate Data Unhandled", k, v);
        failwith "TODO:  Mutate Data Unhandled")
   | ((Event (_newName, _newHandlerType, _newCache))[@implicit_arity ]) as
       _newProp -> failwith "This will never be called because it is gated"
   | ((Style (s))[@explicit_arity ]) as _newProp ->
       (((match oldProp with
          | ((Style (oldS))[@explicit_arity ]) ->
              List.fold_left2
                (fun () ->
                   fun (ok, ov) ->
                     fun (nk, nv) ->
                       if ok = nk
                       then
                         (if ov = nv
                          then ()
                          else
                            Web.Node.setStyleProperty elem nk
                              (Js.Null.return nv))
                       else
                         (let () =
                            Web.Node.setStyleProperty elem ok Js.Null.empty in
                          Web.Node.setStyleProperty elem nk
                            (Js.Null.return nv))) () oldS s
          | _ ->
              failwith
                "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"))
       [@ocaml.warning "-4"]) : 'msg property -> unit)
let rec patchVNodesOnElems_PropertiesApply
  (callbacks : 'msg applicationCallbacks ref) (elem : Web.Node.t) (idx : int)
  (oldProperties : 'msg property list) (newProperties : 'msg property list) =
  (((match (oldProperties, newProperties) with
     | ([], []) -> true
     | ([], _newProp::_newRest) -> false
     | (_oldProp::_oldRest, []) -> false
     | ((NoProp)::oldRest, (NoProp)::newRest) ->
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest
     | ((((RawProp (oldK, oldV))[@implicit_arity ]) as oldProp)::oldRest,
        (((RawProp (newK, newV))[@implicit_arity ]) as newProp)::newRest) ->
         let () =
           if (oldK = newK) && (oldV = newV)
           then ()
           else
             patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx
               oldProp newProp in
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest
     | ((((Attribute (oldNS, oldK, oldV))[@implicit_arity ]) as oldProp)::oldRest,
        (((Attribute (newNS, newK, newV))[@implicit_arity ]) as newProp)::newRest)
         ->
         let () =
           if (oldNS = newNS) && ((oldK = newK) && (oldV = newV))
           then ()
           else
             patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx
               oldProp newProp in
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest
     | ((((Data (oldK, oldV))[@implicit_arity ]) as oldProp)::oldRest,
        (((Data (newK, newV))[@implicit_arity ]) as newProp)::newRest) ->
         let () =
           if (oldK = newK) && (oldV = newV)
           then ()
           else
             patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx
               oldProp newProp in
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest
     | ((((Event (oldName, oldHandlerType, oldCache))[@implicit_arity ]) as
           _oldProp)::oldRest,
        (((Event (newName, newHandlerType, newCache))[@implicit_arity ]) as
           _newProp)::newRest)
         ->
         let () =
           eventHandler_Mutate callbacks elem oldName newName oldHandlerType
             newHandlerType oldCache newCache in
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest
     | ((((Style (oldS))[@explicit_arity ]) as oldProp)::oldRest,
        (((Style (newS))[@explicit_arity ]) as newProp)::newRest) ->
         let () =
           if oldS = newS
           then ()
           else
             patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx
               oldProp newProp in
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest
     | (oldProp::oldRest, newProp::newRest) ->
         let () =
           patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx
             oldProp newProp in
         patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
           newRest)
  [@ocaml.warning "-4"]) : bool)
let patchVNodesOnElems_Properties (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (oldProperties : 'msg property list)
  (newProperties : 'msg property list) =
  (patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties
     newProperties : bool)
let genEmptyProps (length : int) =
  (let rec aux lst =
     function | 0 -> lst | len -> aux (noProp :: lst) (len - 1) in
   aux [] length : 'msg property list)
let mapEmptyProps (props : 'msg property list) =
  (List.map (fun _ -> noProp) props : 'msg property list)
let rec patchVNodesOnElems_ReplaceNode
  (callbacks : 'msg applicationCallbacks ref) (elem : Web.Node.t)
  (elems : Web.Node.t array) (idx : int) =
  (((function
     | ((Node
         (newNamespace, newTagName, _newKey, _newUnique, newProperties,
          newChildren))[@implicit_arity ])
         ->
         let oldChild = elems.(idx) in
         let newChild =
           Web.Document.createElementNsOptional newNamespace newTagName in
         let true =
           patchVNodesOnElems_Properties callbacks newChild
             (mapEmptyProps newProperties) newProperties[@@ocaml.warning
                                                          "-8"] in
         let childChildren = Web.Node.childNodes newChild in
         let () =
           patchVNodesOnElems callbacks newChild childChildren 0 []
             newChildren in
         let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
         let _removedChild = Web.Node.removeChild elem oldChild in ()
     | _ ->
         failwith
           "Node replacement should never be passed anything but a node itself")
  [@ocaml.warning "-4"]) : 'msg t -> unit)
and patchVNodesOnElems_CreateElement
  (callbacks : 'msg applicationCallbacks ref) =
  (function
   | ((CommentNode (s))[@explicit_arity ]) -> Web.Document.createComment s
   | ((Text (text))[@explicit_arity ]) -> Web.Document.createTextNode text
   | ((Node
       (newNamespace, newTagName, _newKey, _unique, newProperties,
        newChildren))[@implicit_arity ])
       ->
       let newChild =
         Web.Document.createElementNsOptional newNamespace newTagName in
       let true =
         patchVNodesOnElems_Properties callbacks newChild
           (mapEmptyProps newProperties) newProperties[@@ocaml.warning "-8"] in
       let childChildren = Web.Node.childNodes newChild in
       let () =
         patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren in
       newChild
   | ((LazyGen (_newKey, newGen, newCache))[@implicit_arity ]) ->
       let vdom = newGen () in
       let () = newCache := vdom in
       patchVNodesOnElems_CreateElement callbacks vdom
   | ((Tagger (tagger, vdom))[@implicit_arity ]) ->
       patchVNodesOnElems_CreateElement (tagger callbacks) vdom : 'msg t ->
                                                                    Web.Node.t)
and patchVNodesOnElems_MutateNode (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (elems : Web.Node.t array) (idx : int)
  (oldNode : 'msg t) (newNode : 'msg t) =
  (match (oldNode, newNode) with
   | ((((Node
         (_oldNamespace, oldTagName, _oldKey, oldUnique, oldProperties,
          oldChildren))[@implicit_arity ])
         as _oldNode),
      (((Node
         (_newNamespace, newTagName, _newKey, newUnique, newProperties,
          newChildren))[@implicit_arity ])
         as newNode))
       ->
       if (oldUnique <> newUnique) || (oldTagName <> newTagName)
       then patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
       else
         (let child = elems.(idx) in
          let childChildren = Web.Node.childNodes child in
          let () =
            if
              patchVNodesOnElems_Properties callbacks child oldProperties
                newProperties
            then ()
            else
              (let () =
                 Js.log
                   "VDom:  Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure.  This is a massive inefficiency until this issue is resolved." in
               patchVNodesOnElems_ReplaceNode callbacks elem elems idx
                 newNode) in
          patchVNodesOnElems callbacks child childChildren 0 oldChildren
            newChildren)
   | _ -> failwith "Non-node passed to patchVNodesOnElems_MutateNode" : 
  unit)
and patchVNodesOnElems (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (elems : Web.Node.t array) (idx : int)
  (oldVNodes : 'msg t list) (newVNodes : 'msg t list) =
  (((match (oldVNodes, newVNodes) with
     | (((Tagger (_oldTagger, oldVdom))[@implicit_arity ])::oldRest, _) ->
         patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest)
           newVNodes
     | (oldNode::oldRest, ((Tagger
        (newTagger, newVdom))[@implicit_arity ])::newRest) ->
         let () =
           patchVNodesOnElems (newTagger callbacks) elem elems idx [oldNode]
             [newVdom] in
         patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
     | ([], []) -> ()
     | ([], newNode::newRest) ->
         let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
         let _attachedChild = Web.Node.appendChild elem newChild in
         patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
     | (_oldVnode::oldRest, []) ->
         let child = elems.(idx) in
         let _removedChild = Web.Node.removeChild elem child in
         patchVNodesOnElems callbacks elem elems idx oldRest []
     | (((CommentNode (oldS))[@explicit_arity ])::oldRest, ((CommentNode
        (newS))[@explicit_arity ])::newRest) when oldS = newS ->
         patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
     | (((Text (oldText))[@explicit_arity ])::oldRest, ((Text
        (newText))[@explicit_arity ])::newRest) ->
         let () =
           if oldText = newText
           then ()
           else
             (let child = elems.(idx) in Web.Node.set_nodeValue child newText) in
         patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
     | (((LazyGen (oldKey, _oldGen, oldCache))[@implicit_arity ])::oldRest,
        ((LazyGen (newKey, newGen, newCache))[@implicit_arity ])::newRest) ->
         if oldKey = newKey
         then
           let () = newCache := (!oldCache) in
           patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
         else
           (match (oldRest, newRest) with
            | (((LazyGen
               (olderKey, _olderGen, _olderCache))[@implicit_arity ])::olderRest,
               ((LazyGen
               (newerKey, _newerGen, _newerCache))[@implicit_arity ])::newerRest)
                when (olderKey = newKey) && (oldKey = newerKey) ->
                let firstChild = elems.(idx) in
                let secondChild = elems.(idx + 1) in
                let _removedChild = Web.Node.removeChild elem secondChild in
                let _attachedChild =
                  Web.Node.insertBefore elem secondChild firstChild in
                patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
                  newerRest
            | (((LazyGen
               (olderKey, _olderGen, olderCache))[@implicit_arity ])::olderRest,
               _) when olderKey = newKey ->
                let oldChild = elems.(idx) in
                let _removedChild = Web.Node.removeChild elem oldChild in
                let oldVdom = !olderCache in
                let () = newCache := oldVdom in
                patchVNodesOnElems callbacks elem elems (idx + 1) olderRest
                  newRest
            | (_, ((LazyGen
               (newerKey, _newerGen, _newerCache))[@implicit_arity ])::_newerRest)
                when newerKey = oldKey ->
                let oldChild = elems.(idx) in
                let newVdom = newGen () in
                let () = newCache := newVdom in
                let newChild =
                  patchVNodesOnElems_CreateElement callbacks newVdom in
                let _attachedChild =
                  Web.Node.insertBefore elem newChild oldChild in
                patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes
                  newRest
            | _ ->
                let oldVdom = !oldCache in
                let newVdom = newGen () in
                let () = newCache := newVdom in
                patchVNodesOnElems callbacks elem elems idx (oldVdom ::
                  oldRest) (newVdom :: newRest))
     | ((((Node
           (oldNamespace, oldTagName, oldKey, _oldUnique, _oldProperties,
            _oldChildren))[@implicit_arity ])
           as oldNode)::oldRest,
        (((Node
           (newNamespace, newTagName, newKey, _newUnique, _newProperties,
            _newChildren))[@implicit_arity ])
           as newNode)::newRest)
         ->
         if (oldKey = newKey) && (oldKey <> "")
         then
           patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
         else
           if (oldKey = "") || (newKey = "")
           then
             (let () =
                patchVNodesOnElems_MutateNode callbacks elem elems idx
                  oldNode newNode in
              patchVNodesOnElems callbacks elem elems (idx + 1) oldRest
                newRest)
           else
             (match (oldRest, newRest) with
              | (((Node
                 (olderNamespace, olderTagName, olderKey, _olderUnique,
                  _olderProperties, _olderChildren))[@implicit_arity ])::olderRest,
                 ((Node
                 (newerNamespace, newerTagName, newerKey, _newerUnique,
                  _newerProperties, _newerChildren))[@implicit_arity ])::newerRest)
                  when
                  (olderNamespace = newNamespace) &&
                    ((olderTagName = newTagName) &&
                       ((olderKey = newKey) &&
                          ((oldNamespace = newerNamespace) &&
                             ((oldTagName = newerTagName) &&
                                (oldKey = newerKey)))))
                  ->
                  let firstChild = elems.(idx) in
                  let secondChild = elems.(idx + 1) in
                  let _removedChild = Web.Node.removeChild elem secondChild in
                  let _attachedChild =
                    Web.Node.insertBefore elem secondChild firstChild in
                  patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
                    newerRest
              | (((Node
                 (olderNamespace, olderTagName, olderKey, _olderUnique,
                  _olderProperties, _olderChildren))[@implicit_arity ])::olderRest,
                 _) when
                  (olderNamespace = newNamespace) &&
                    ((olderTagName = newTagName) && (olderKey = newKey))
                  ->
                  let oldChild = elems.(idx) in
                  let _removedChild = Web.Node.removeChild elem oldChild in
                  patchVNodesOnElems callbacks elem elems (idx + 1) olderRest
                    newRest
              | (_, ((Node
                 (newerNamespace, newerTagName, newerKey, _newerUnique,
                  _newerProperties, _newerChildren))[@implicit_arity ])::_newerRest)
                  when
                  (oldNamespace = newerNamespace) &&
                    ((oldTagName = newerTagName) && (oldKey = newerKey))
                  ->
                  let oldChild = elems.(idx) in
                  let newChild =
                    patchVNodesOnElems_CreateElement callbacks newNode in
                  let _attachedChild =
                    Web.Node.insertBefore elem newChild oldChild in
                  patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes
                    newRest
              | _ ->
                  let () =
                    patchVNodesOnElems_MutateNode callbacks elem elems idx
                      oldNode newNode in
                  patchVNodesOnElems callbacks elem elems (idx + 1) oldRest
                    newRest)
     | (_oldVnode::oldRest, newNode::newRest) ->
         let oldChild = elems.(idx) in
         let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
         let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
         let _removedChild = Web.Node.removeChild elem oldChild in
         patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest)
  [@ocaml.warning "-4"]) : unit)
let patchVNodesIntoElement (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (oldVNodes : 'msg t list) (newVNodes : 'msg t list) =
  (let elems = Web.Node.childNodes elem in
   let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in
   newVNodes : 'msg t list)
let patchVNodeIntoElement (callbacks : 'msg applicationCallbacks ref)
  (elem : Web.Node.t) (oldVNode : 'msg t) (newVNode : 'msg t) =
  (patchVNodesIntoElement callbacks elem [oldVNode] [newVNode] : 'msg t list)
let wrapCallbacks_On : type a b.
  (a -> b) -> a systemMessage -> b systemMessage =
  fun func ->
    function
    | Render -> Render
    | ((AddRenderMsg (msg))[@explicit_arity ]) ->
        ((AddRenderMsg ((func msg)))[@explicit_arity ])
    | ((RemoveRenderMsg (msg))[@explicit_arity ]) ->
        ((RemoveRenderMsg ((func msg)))[@explicit_arity ])
let wrapCallbacks : type a b.
  (a -> b) -> b applicationCallbacks ref -> a applicationCallbacks ref =
  fun func ->
    fun callbacks ->
      Obj.magic ref
        {
          enqueue =
            (fun (msg : a) ->
               let new_msg = func msg in (!callbacks).enqueue new_msg);
          on =
            (fun smsg ->
               let new_smsg = wrapCallbacks_On func smsg in
               (!callbacks).on new_smsg)
        }
let map =
  ((fun func ->
      fun vdom ->
        let tagger = wrapCallbacks func in
        ((Tagger ((Obj.magic tagger), (Obj.magic vdom)))[@implicit_arity ]) : 
  ('a -> 'b) -> 'a t -> 'b t) : ('a -> 'b) -> 'a t -> 'b t)