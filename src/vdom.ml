type eventCallback = Dom.event -> unit

type 'msg systemMessage =
  | Render
  | AddRenderMsg of 'msg
  | RemoveRenderMsg of 'msg

type 'msg applicationCallbacks =
  {
  enqueue: 'msg -> unit ;
  on: 'msg systemMessage -> unit }

type 'msg eventHandler =
  | EventHandlerCallback of string * (Dom.event -> 'msg option)
  | EventHandlerMsg of 'msg

type 'msg eventCache =
  {
  handler: eventCallback ;
  cb: (Dom.event -> 'msg option) ref }

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

let onCB (name : string) (key : string) (cb : Dom.event -> 'msg option)
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

let createElementNsOptional namespace tagName =
  let document = Webapi.Dom.document in
  match namespace with
  | "" -> Webapi.Dom.Document.createElement document tagName
  | ns -> Webapi.Dom.Document.createElementNS document ns tagName

let nodeAt (index : int) (nodes : Dom.nodeList) : Dom.node =
  Webapi.Dom.NodeList.item nodes index |> Belt.Option.getExn

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
  ) : eventCallback) : eventCallback)

let emptyEventCB _ev = (None : eventCallback option)

let eventHandler (callbacks : 'msg applicationCallbacks ref)
  (cb : (Dom.event -> 'msg option) ref) =
  (((fun ev ->
       match (!cb) ev with
       | None -> ()
       | ((Some (msg))[@explicit_arity ]) -> (!callbacks).enqueue msg)
  ) : eventCallback)

let eventHandler_GetCB =
  ((function
    | ((EventHandlerCallback (_, cb))[@implicit_arity ]) -> cb
    | ((EventHandlerMsg (msg))[@explicit_arity ]) ->
        (fun _ev -> ((Some (msg))[@explicit_arity ])) : 'msg eventHandler ->
                                                          Dom.event ->
                                                            'msg option) :
  'msg eventHandler -> Dom.event -> 'msg option)

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
  (elem : Dom.eventTarget) (name : string) (handlerType : 'msg eventHandler) =
  (let cb = ref (eventHandler_GetCB handlerType) in
   let handler = eventHandler callbacks cb in
   let () = Webapi.Dom.EventTarget.addEventListener elem name handler in
   ((Some ({ handler; cb }))[@explicit_arity ]) : 'msg eventCache option)

let eventHandler_Unregister (elem : Dom.eventTarget) (name : string) =
  (function
   | None -> None
   | ((Some (cache))[@explicit_arity ]) ->
       let () = Webapi.Dom.EventTarget.removeEventListener elem name cache.handler in
       None : 'msg eventCache option -> 'msg eventCache option)

let eventHandler_Mutate (callbacks : 'msg applicationCallbacks ref)
  (elem : Dom.eventTarget) (oldName : string) (newName : string)
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
  (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
  (_idx : int) =
  (function
   | NoProp -> ()
   | ((RawProp (k, v))[@implicit_arity ]) ->
      Vdom2.setItem elem k v
   | ((Attribute (namespace, k, v))[@implicit_arity ]) ->
       Webapi.Dom.Element.setAttributeNS elem namespace k v
   | ((Data (k, v))[@implicit_arity ]) ->
       (Js.log ("TODO:  Add Data Unhandled", k, v);
        failwith "TODO:  Add Data Unhandled")
   | ((Event (name, handlerType, cache))[@implicit_arity ]) ->
       let eventTarget = Webapi.Dom.Element.asEventTarget elem in
       cache := (eventHandler_Register callbacks eventTarget name handlerType)
   | ((Style (s))[@explicit_arity ]) ->
       (match Webapi.Dom.HtmlElement.ofElement elem with
       | Some(elem) ->
         let elemStyle = Webapi.Dom.HtmlElement.style elem in
         List.fold_left
           (fun () ->
              fun (k, v) -> Vdom2.setPropertyValue elemStyle k v)
           () s
      | None -> failwith "Expected htmlelement in patchVNodesOnElems_PropertiesApply_Add"))

let patchVNodesOnElems_PropertiesApply_Remove
  (_callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
  (_idx : int) =
  (function
   | NoProp -> ()
   | ((RawProp (k, _v))[@implicit_arity ]) ->
       Vdom2.delItem elem k
   | ((Attribute (namespace, k, _v))[@implicit_arity ]) ->
       Webapi.Dom.Element.removeAttributeNS elem namespace k
   | ((Data (k, v))[@implicit_arity ]) ->
       (Js.log ("TODO:  Remove Data Unhandled", k, v);
        failwith "TODO:  Remove Data Unhandled")
   | ((Event (name, _, cache))[@implicit_arity ]) ->
       let eventTarget = Webapi.Dom.Element.asEventTarget elem in
       cache := (eventHandler_Unregister eventTarget name (!cache))
   | ((Style (s))[@explicit_arity ]) ->
       (match Webapi.Dom.HtmlElement.ofElement elem with
       | Some(elem) ->
         let elemStyle = Webapi.Dom.HtmlElement.style elem in
         List.fold_left
           (fun () ->
              fun (k, _v) -> Webapi.Dom.CssStyleDeclaration.removeProperty elemStyle k |> ignore)
           () s
      | None -> failwith "Expected htmlelement in patchVNodesOnElems_PropertiesApply_Remove"))

let patchVNodesOnElems_PropertiesApply_RemoveAdd
  (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (idx : int)
  (oldProp : 'msg property) (newProp : 'msg property) =
  (let () =
     patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp in
   let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
   () : unit)

let patchVNodesOnElems_PropertiesApply_Mutate
  (_callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
  (_idx : int) (oldProp : 'msg property) =
  (function
   | NoProp ->
       failwith
         "This should never be called as all entries through NoProp are gated."
   | ((RawProp (k, v))[@implicit_arity ]) ->
       Vdom2.setItem elem k v
   | ((Attribute (namespace, k, v))[@implicit_arity ]) ->
       Webapi.Dom.Element.setAttributeNS elem namespace k v
   | ((Data (k, v))[@implicit_arity ]) ->
       (Js.log ("TODO:  Mutate Data Unhandled", k, v);
        failwith "TODO:  Mutate Data Unhandled")
   | ((Event (_newName, _newHandlerType, _newCache))[@implicit_arity ])
       -> failwith "This will never be called because it is gated"
   | ((Style (s))[@explicit_arity ]) ->
     (match Webapi.Dom.HtmlElement.ofElement elem with
      | None -> failwith "Expected htmlelement in patchVNodesOnElems_PropertiesApply_Mutate"
      | Some(elem) ->
        let elemStyle = Webapi.Dom.HtmlElement.style elem in
        (match oldProp with
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
                            Webapi.Dom.CssStyleDeclaration.setPropertyValue elemStyle nk nv)
                       else
                         (let (_ : string) =
                            Webapi.Dom.CssStyleDeclaration.removeProperty elemStyle ok in
                          Webapi.Dom.CssStyleDeclaration.setPropertyValue elemStyle nk nv))
                   () oldS s
          | _ ->
              failwith
                "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!")))

let rec patchVNodesOnElems_PropertiesApply
  (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (idx : int)
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
         let eventTarget = Webapi.Dom.Element.asEventTarget elem in
         let () =
           eventHandler_Mutate callbacks eventTarget oldName newName oldHandlerType
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
  (elem : Dom.element) (oldProperties : 'msg property list)
  (newProperties : 'msg property list) : bool =
  (patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties newProperties)

let genEmptyProps (length : int)  : 'msg property list =
  (let rec aux lst =
     function | 0 -> lst | len -> aux (noProp :: lst) (len - 1) in
   aux [] length)

let mapEmptyProps (props : 'msg property list) : 'msg property list =
  (List.map (fun _ -> noProp) props)

let rec patchVNodesOnElems_ReplaceNode
  (callbacks : 'msg applicationCallbacks ref) (elem : Dom.node)
  (elems : Dom.nodeList) (idx : int) =
  (((function
     | ((Node
         (newNamespace, newTagName, _newKey, _newUnique, newProperties,
          newChildren))[@implicit_arity ])
         ->
         let oldChild = nodeAt idx elems in
         let newChild = createElementNsOptional newNamespace newTagName in
         let _ : bool =
           patchVNodesOnElems_Properties callbacks newChild
             (mapEmptyProps newProperties) newProperties in
         let newChildNode = Webapi.Dom.Element.asNode newChild in
         let childChildren = Webapi.Dom.Node.childNodes newChildNode in
         let () =
           patchVNodesOnElems callbacks newChildNode childChildren 0 []
             newChildren in
         let _attachedChild = Vdom2.insertBefore elem ~new_:newChildNode ~before:oldChild in
         let _removedChild = Webapi.Dom.Node.removeChild elem ~child:oldChild in ()
     | _ ->
         failwith
           "Node replacement should never be passed anything but a node itself")) : 'msg t -> unit)

and patchVNodesOnElems_CreateElement
  (callbacks : 'msg applicationCallbacks ref) =
  (function
   | ((CommentNode (s))[@explicit_arity ]) ->
     (Webapi.Dom.Document.createComment Webapi.Dom.document s)
     |> Webapi.Dom.Comment.asNode
   | ((Text (text))[@explicit_arity ]) ->
     Webapi.Dom.Document.createTextNode Webapi.Dom.document text |> Webapi.Dom.Text.asNode
   | ((Node
       (newNamespace, newTagName, _newKey, _unique, newProperties,
        newChildren))[@implicit_arity ])
       ->
       let newChild =
         createElementNsOptional newNamespace newTagName in
       let true =
         patchVNodesOnElems_Properties callbacks newChild
           (mapEmptyProps newProperties) newProperties[@@ocaml.warning "-8"] in
       let newChildNode = Webapi.Dom.Element.asNode newChild in
       let childChildren = Webapi.Dom.Node.childNodes newChildNode in
       let () =
         patchVNodesOnElems callbacks newChildNode childChildren 0 [] newChildren in
       newChildNode
   | ((LazyGen (_newKey, newGen, newCache))[@implicit_arity ]) ->
       let vdom = newGen () in
       let () = newCache := vdom in
       patchVNodesOnElems_CreateElement callbacks vdom
   | ((Tagger (tagger, vdom))[@implicit_arity ]) ->
       patchVNodesOnElems_CreateElement (tagger callbacks) vdom : 'msg t ->
                                                                    Dom.node)

and patchVNodesOnElems_MutateNode (callbacks : 'msg applicationCallbacks ref)
  (elem : Dom.node) (elems : Dom.nodeList) (idx : int)
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
         (let child = nodeAt idx elems in
          match Webapi.Dom.Element.ofNode child with
          | None -> failwith "Expected element in patchVNodesOnElems_MutateNode"
          | Some(childElement) ->
            let childChildren = Webapi.Dom.Node.childNodes child in
            let () =
              if
                patchVNodesOnElems_Properties callbacks childElement oldProperties
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
  (elem : Dom.node) (elems : Dom.nodeList) (idx : int)
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
         let _attachedChild = Webapi.Dom.Node.appendChild elem ~child:newChild in
         patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
     | (_oldVnode::oldRest, []) ->
         let child = nodeAt idx elems in
         let _removedChild = Webapi.Dom.Node.removeChild elem ~child in
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
             (let child = nodeAt idx elems in
              Webapi.Dom.Node.setNodeValue child (Js.Null.return newText)) in
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
                let firstChild = nodeAt idx elems in
                let secondChild = nodeAt (idx + 1) elems in
                let _removedChild = Webapi.Dom.Node.removeChild elem ~child:secondChild in
                let _attachedChild =
                  Vdom2.insertBefore elem ~new_:secondChild ~before:firstChild in
                patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
                  newerRest
            | (((LazyGen
               (olderKey, _olderGen, olderCache))[@implicit_arity ])::olderRest,
               _) when olderKey = newKey ->
                let oldChild = nodeAt idx elems in
                let _removedChild = Webapi.Dom.Node.removeChild elem ~child:oldChild in
                let oldVdom = !olderCache in
                let () = newCache := oldVdom in
                patchVNodesOnElems callbacks elem elems (idx + 1) olderRest
                  newRest
            | (_, ((LazyGen
               (newerKey, _newerGen, _newerCache))[@implicit_arity ])::_newerRest)
                when newerKey = oldKey ->
                let oldChild = nodeAt idx elems in
                let newVdom = newGen () in
                let () = newCache := newVdom in
                let newChild =
                  patchVNodesOnElems_CreateElement callbacks newVdom in
                let _attachedChild =
                  Vdom2.insertBefore elem ~new_:newChild ~before:oldChild in
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
                  let firstChild = nodeAt idx elems in
                  let secondChild = nodeAt (idx + 1) elems in
                  let _removedChild = Webapi.Dom.Node.removeChild elem ~child:secondChild in
                  let _attachedChild =
                    Vdom2.insertBefore elem ~new_:secondChild ~before:firstChild in
                  patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
                    newerRest
              | (((Node
                 (olderNamespace, olderTagName, olderKey, _olderUnique,
                  _olderProperties, _olderChildren))[@implicit_arity ])::olderRest,
                 _) when
                  (olderNamespace = newNamespace) &&
                    ((olderTagName = newTagName) && (olderKey = newKey))
                  ->
                  let oldChild = nodeAt idx elems in
                  let _removedChild = Webapi.Dom.Node.removeChild elem ~child:oldChild in
                  patchVNodesOnElems callbacks elem elems (idx + 1) olderRest
                    newRest
              | (_, ((Node
                 (newerNamespace, newerTagName, newerKey, _newerUnique,
                  _newerProperties, _newerChildren))[@implicit_arity ])::_newerRest)
                  when
                  (oldNamespace = newerNamespace) &&
                    ((oldTagName = newerTagName) && (oldKey = newerKey))
                  ->
                  let oldChild = nodeAt idx elems in
                  let newChild =
                    patchVNodesOnElems_CreateElement callbacks newNode in
                  let _attachedChild =
                    Vdom2.insertBefore elem ~new_:newChild ~before:oldChild in
                  patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes
                    newRest
              | _ ->
                  let () =
                    patchVNodesOnElems_MutateNode callbacks elem elems idx
                      oldNode newNode in
                  patchVNodesOnElems callbacks elem elems (idx + 1) oldRest
                    newRest)
     | (_oldVnode::oldRest, newNode::newRest) ->
         let oldChild = nodeAt idx elems in
         let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
         let _attachedChild = Vdom2.insertBefore elem ~new_:newChild ~before:oldChild in
         let _removedChild = Webapi.Dom.Node.removeChild elem ~child:oldChild in
         patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest)
  [@ocaml.warning "-4"]) : unit)

let patchVNodesIntoElement (callbacks : 'msg applicationCallbacks ref)
  (elem : Dom.node) (oldVNodes : 'msg t list) (newVNodes : 'msg t list) =
  (let elems = Webapi.Dom.Node.childNodes elem in
   let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in
   newVNodes : 'msg t list)

let patchVNodeIntoElement (callbacks : 'msg applicationCallbacks ref)
  (elem : Dom.node) (oldVNode : 'msg t) (newVNode : 'msg t) =
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