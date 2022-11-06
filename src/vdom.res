type eventCallback = Dom.event => unit

type systemMessage<'msg> =
  | Render
  | AddRenderMsg('msg)
  | RemoveRenderMsg('msg)

type applicationCallbacks<'msg> = {
  enqueue: 'msg => unit,
  on: systemMessage<'msg> => unit,
}

type eventHandler<'msg> =
  | EventHandlerCallback(string, Dom.event => option<'msg>)
  | EventHandlerMsg('msg)

type eventCache<'msg> = {
  handler: eventCallback,
  cb: ref<Dom.event => option<'msg>>,
}

type property<'msg> =
  | NoProp
  | RawProp(string, string)
  | Attribute(string, string, string)
  | Data(string, string)
  | Event(string, eventHandler<'msg>, ref<option<eventCache<'msg>>>)
  | Style(list<(string, string)>)

type properties<'msg> = list<property<'msg>>

type rec t<'msg> =
  | CommentNode(string)
  | Text(string)
  | Node(string, string, string, string, properties<'msg>, list<t<'msg>>)

  | LazyGen(string, unit => t<'msg>, ref<t<'msg>>)
  | Tagger(ref<applicationCallbacks<'msg>> => ref<applicationCallbacks<'msg>>, t<'msg>)

let noNode: t<'msg> = (CommentNode(""): t<'msg>)

let comment = (s: string): t<'msg> => CommentNode(s)

let text = (s: string): t<'msg> => Text(s)

let fullnode = (
  namespace: string,
  tagName: string,
  key: string,
  unique: string,
  props: properties<'msg>,
  vdoms: list<t<'msg>>,
): t<'msg> => @implicit_arity Node(namespace, tagName, key, unique, props, vdoms)

let node = (
  ~namespace: string="",
  tagName: string,
  ~key: string="",
  ~unique: string="",
  props: properties<'msg>,
  vdoms: list<t<'msg>>,
): t<'msg> => fullnode(namespace, tagName, key, unique, props, vdoms)

let lazyGen = (key: string, fn: unit => t<'msg>): t<'msg> => @implicit_arity
LazyGen(key, fn, ref(noNode))

let noProp: property<'msg> = (NoProp: property<'msg>)

let prop = (key: string, value: string): property<'msg> => @implicit_arity RawProp(key, value)

let onCB = (name: string, key: string, cb: Dom.event => option<'msg>): property<
  'msg,
> => @implicit_arity Event(name, @implicit_arity EventHandlerCallback(key, cb), ref(None))

let onMsg = (name: string, msg: 'msg): property<'msg> => @implicit_arity
Event(name, EventHandlerMsg(msg), ref(None))

let attribute = (namespace: string, key: string, value: string): property<'msg> => @implicit_arity
Attribute(namespace, key, value)

let data = (key: string, value: string): property<'msg> => @implicit_arity Data(key, value)

let style = (key: string, value: string): property<'msg> => Style(list{(key, value)})

let styles = (s): property<'msg> => Style(s)

let createElementNsOptional = (namespace, tagName) => {
  let document = Webapi.Dom.document
  switch namespace {
  | "" => Webapi.Dom.Document.createElement(document, tagName)
  | ns => Webapi.Dom.Document.createElementNS(document, ns, tagName)
  }
}

let nodeAt = (index: int, nodes: Dom.nodeList): Dom.node =>
  Webapi.Dom.NodeList.item(nodes, index) |> Belt.Option.getExn

let rec renderToHtmlString: t<'msg> => string = (
  x =>
    switch x {
    | CommentNode(s) => "<!-- " ++ (s ++ " -->")
    | Text(s) => s
    | @implicit_arity Node(namespace, tagName, _key, _unique, props, vdoms) =>
      let renderProp = x =>
        switch x {
        | NoProp => ""
        | @implicit_arity RawProp(k, v) => String.concat("", list{" ", k, "=\"", v, "\""})
        | @implicit_arity Attribute(_namespace, k, v) =>
          String.concat("", list{" ", k, "=\"", v, "\""})
        | @implicit_arity Data(k, v) => String.concat("", list{" data-", k, "=\"", v, "\""})
        | @implicit_arity Event(_, _, _) => ""
        | Style(s) =>
          String.concat(
            "",
            list{
              " style=\"",
              String.concat(";", List.map(((k, v)) => String.concat("", list{k, ":", v, ";"}), s)),
              "\"",
            },
          )
        }
      String.concat(
        "",
        list{
          "<",
          namespace,
          if namespace == "" {
            ""
          } else {
            ":"
          },
          tagName,
          String.concat("", List.map(p => renderProp(p), props)),
          ">",
          String.concat("", List.map(v => renderToHtmlString(v), vdoms)),
          "</",
          tagName,
          ">",
        },
      )
    | @implicit_arity LazyGen(_key, gen, _cache) =>
      let vdom = gen()
      renderToHtmlString(vdom)
    | @implicit_arity Tagger(_tagger, vdom) => renderToHtmlString(vdom)
    }: t<'msg> => string
)

let emptyEventHandler: eventCallback = (_ev => (): eventCallback)

let emptyEventCB = (_ev): option<eventCallback> => None

let eventHandler = (
  callbacks: ref<applicationCallbacks<'msg>>,
  cb: ref<Dom.event => option<'msg>>,
): eventCallback =>
  ev =>
    switch cb.contents(ev) {
    | None => ()
    | Some(msg) => callbacks.contents.enqueue(msg)
    }

let eventHandler_GetCB: (eventHandler<'msg>, Dom.event) => option<'msg> = (
  x =>
    switch x {
    | @implicit_arity EventHandlerCallback(_, cb) => cb
    | EventHandlerMsg(msg) => _ev => Some(msg)
    }: (eventHandler<'msg>, Dom.event) => option<'msg>
)

let compareEventHandlerTypes = (left: eventHandler<'msg>): (eventHandler<'msg> => bool) =>
  x =>
    switch x {
    | @implicit_arity EventHandlerCallback(cb, _) =>
      switch left {
      | @implicit_arity EventHandlerCallback(lcb, _) if cb == lcb => true
      | _ => false
      }
    | EventHandlerMsg(msg) =>
      switch left {
      | EventHandlerMsg(lmsg) if msg == lmsg => true
      | _ => false
      }
    }

let eventHandler_Register = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.eventTarget,
  name: string,
  handlerType: eventHandler<'msg>,
): option<eventCache<'msg>> => {
  let cb = ref(eventHandler_GetCB(handlerType))
  let handler = eventHandler(callbacks, cb)
  let () = Webapi.Dom.EventTarget.addEventListener(elem, name, handler)
  Some({handler: handler, cb: cb})
}

let eventHandler_Unregister = (elem: Dom.eventTarget, name: string): (
  option<eventCache<'msg>> => option<eventCache<'msg>>
) =>
  x =>
    switch x {
    | None => None
    | Some(cache) =>
      let () = Webapi.Dom.EventTarget.removeEventListener(elem, name, cache.handler)
      None
    }

let eventHandler_Mutate = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.eventTarget,
  oldName: string,
  newName: string,
  oldHandlerType: eventHandler<'msg>,
  newHandlerType: eventHandler<'msg>,
  oldCache: ref<option<eventCache<'msg>>>,
  newCache: ref<option<eventCache<'msg>>>,
): unit =>
  switch oldCache.contents {
  | None => newCache := eventHandler_Register(callbacks, elem, newName, newHandlerType)
  | Some(oldcache) =>
    if oldName == newName {
      let () = newCache := oldCache.contents
      if compareEventHandlerTypes(oldHandlerType, newHandlerType) {
        ()
      } else {
        let cb = eventHandler_GetCB(newHandlerType)
        let () = oldcache.cb := cb
      }
    } else {
      let () = oldCache := eventHandler_Unregister(elem, oldName, oldCache.contents)
      let () = newCache := eventHandler_Register(callbacks, elem, newName, newHandlerType)
    }
  }

let patchVNodesOnElems_PropertiesApply_Add = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.element,
  _idx: int,
  x,
) =>
  switch x {
  | NoProp => ()
  | @implicit_arity RawProp(k, v) => Vdom2.setItem(elem, k, v)
  | @implicit_arity Attribute(namespace, k, v) =>
    Webapi.Dom.Element.setAttributeNS(elem, namespace, k, v)
  | @implicit_arity Data(k, v) =>
    Js.log(("TODO:  Add Data Unhandled", k, v))
    failwith("TODO:  Add Data Unhandled")
  | @implicit_arity Event(name, handlerType, cache) =>
    let eventTarget = Webapi.Dom.Element.asEventTarget(elem)
    cache := eventHandler_Register(callbacks, eventTarget, name, handlerType)
  | Style(s) =>
    switch Webapi.Dom.HtmlElement.ofElement(elem) {
    | Some(elem) =>
      let elemStyle = Webapi.Dom.HtmlElement.style(elem)
      List.fold_left(
        ((), (k, v)) => Webapi.Dom.CssStyleDeclaration.setPropertyValue(elemStyle, k, v),
        (),
        s,
      )
    | None => failwith("Expected htmlelement in patchVNodesOnElems_PropertiesApply_Add")
    }
  }

let patchVNodesOnElems_PropertiesApply_Remove = (
  _callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.element,
  _idx: int,
  x,
) =>
  switch x {
  | NoProp => ()
  | @implicit_arity RawProp(k, _v) => Vdom2.delItem(elem, k)
  | @implicit_arity Attribute(namespace, k, _v) =>
    Webapi.Dom.Element.removeAttributeNS(elem, namespace, k)
  | @implicit_arity Data(k, v) =>
    Js.log(("TODO:  Remove Data Unhandled", k, v))
    failwith("TODO:  Remove Data Unhandled")
  | @implicit_arity Event(name, _, cache) =>
    let eventTarget = Webapi.Dom.Element.asEventTarget(elem)
    cache := eventHandler_Unregister(eventTarget, name, cache.contents)
  | Style(s) =>
    switch Webapi.Dom.HtmlElement.ofElement(elem) {
    | Some(elem) =>
      let elemStyle = Webapi.Dom.HtmlElement.style(elem)
      List.fold_left(
        ((), (k, _v)) => Webapi.Dom.CssStyleDeclaration.removeProperty(elemStyle, k) |> ignore,
        (),
        s,
      )
    | None => failwith("Expected htmlelement in patchVNodesOnElems_PropertiesApply_Remove")
    }
  }

let patchVNodesOnElems_PropertiesApply_RemoveAdd = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.element,
  idx: int,
  oldProp: property<'msg>,
  newProp: property<'msg>,
): unit => {
  let () = patchVNodesOnElems_PropertiesApply_Remove(callbacks, elem, idx, oldProp)
  let () = patchVNodesOnElems_PropertiesApply_Add(callbacks, elem, idx, newProp)
}

let patchVNodesOnElems_PropertiesApply_Mutate = (
  _callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.element,
  _idx: int,
  oldProp: property<'msg>,
  x,
) =>
  switch x {
  | NoProp => failwith("This should never be called as all entries through NoProp are gated.")
  | @implicit_arity RawProp(k, v) => Vdom2.setItem(elem, k, v)
  | @implicit_arity Attribute(namespace, k, v) =>
    Webapi.Dom.Element.setAttributeNS(elem, namespace, k, v)
  | @implicit_arity Data(k, v) =>
    Js.log(("TODO:  Mutate Data Unhandled", k, v))
    failwith("TODO:  Mutate Data Unhandled")
  | @implicit_arity Event(_newName, _newHandlerType, _newCache) =>
    failwith("This will never be called because it is gated")
  | Style(s) =>
    switch Webapi.Dom.HtmlElement.ofElement(elem) {
    | None => failwith("Expected htmlelement in patchVNodesOnElems_PropertiesApply_Mutate")
    | Some(elem) =>
      let elemStyle = Webapi.Dom.HtmlElement.style(elem)
      switch oldProp {
      | Style(oldS) => List.fold_left2(((), (ok, ov), (nk, nv)) =>
          if ok == nk {
            if ov == nv {
              ()
            } else {
              Webapi.Dom.CssStyleDeclaration.setPropertyValue(elemStyle, nk, nv)
            }
          } else {
            let _: string = Webapi.Dom.CssStyleDeclaration.removeProperty(elemStyle, ok)
            Webapi.Dom.CssStyleDeclaration.setPropertyValue(elemStyle, nk, nv)
          }
        , (), oldS, s)
      | _ =>
        failwith(
          "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!",
        )
      }
    }
  }

let rec patchVNodesOnElems_PropertiesApply = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.element,
  idx: int,
  oldProperties: list<property<'msg>>,
  newProperties: list<property<'msg>>,
): bool =>
  @ocaml.warning("-4")
  switch (oldProperties, newProperties) {
  | (list{}, list{}) => true
  | (list{}, list{_newProp, ..._newRest}) => false
  | (list{_oldProp, ..._oldRest}, list{}) => false
  | (list{NoProp, ...oldRest}, list{NoProp, ...newRest}) =>
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  | (
      list{@implicit_arity RawProp(oldK, oldV) as oldProp, ...oldRest},
      list{@implicit_arity RawProp(newK, newV) as newProp, ...newRest},
    ) =>
    let () = if oldK == newK && oldV == newV {
      ()
    } else {
      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, oldProp, newProp)
    }
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  | (
      list{@implicit_arity Attribute(oldNS, oldK, oldV) as oldProp, ...oldRest},
      list{@implicit_arity Attribute(newNS, newK, newV) as newProp, ...newRest},
    ) =>
    let () = if oldNS == newNS && (oldK == newK && oldV == newV) {
      ()
    } else {
      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, oldProp, newProp)
    }
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  | (
      list{@implicit_arity Data(oldK, oldV) as oldProp, ...oldRest},
      list{@implicit_arity Data(newK, newV) as newProp, ...newRest},
    ) =>
    let () = if oldK == newK && oldV == newV {
      ()
    } else {
      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, oldProp, newProp)
    }
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  | (
      list{@implicit_arity Event(oldName, oldHandlerType, oldCache) as _oldProp, ...oldRest},
      list{@implicit_arity Event(newName, newHandlerType, newCache) as _newProp, ...newRest},
    ) =>
    let eventTarget = Webapi.Dom.Element.asEventTarget(elem)
    let () = eventHandler_Mutate(
      callbacks,
      eventTarget,
      oldName,
      newName,
      oldHandlerType,
      newHandlerType,
      oldCache,
      newCache,
    )
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  | (list{Style(oldS) as oldProp, ...oldRest}, list{Style(newS) as newProp, ...newRest}) =>
    let () = if oldS == newS {
      ()
    } else {
      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, oldProp, newProp)
    }
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  | (list{oldProp, ...oldRest}, list{newProp, ...newRest}) =>
    let () = patchVNodesOnElems_PropertiesApply_RemoveAdd(callbacks, elem, idx, oldProp, newProp)
    patchVNodesOnElems_PropertiesApply(callbacks, elem, idx + 1, oldRest, newRest)
  }

let patchVNodesOnElems_Properties = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.element,
  oldProperties: list<property<'msg>>,
  newProperties: list<property<'msg>>,
): bool => patchVNodesOnElems_PropertiesApply(callbacks, elem, 0, oldProperties, newProperties)

let genEmptyProps = (length: int): list<property<'msg>> => {
  let rec aux = (lst, x) =>
    switch x {
    | 0 => lst
    | len => aux(list{noProp, ...lst}, len - 1)
    }
  aux(list{}, length)
}

let mapEmptyProps = (props: list<property<'msg>>): list<property<'msg>> =>
  List.map(_ => noProp, props)

let rec patchVNodesOnElems_ReplaceNode = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.node,
  elems: Dom.nodeList,
  idx: int,
): (t<'msg> => unit) =>
  x =>
    switch x {
    | @implicit_arity
      Node(newNamespace, newTagName, _newKey, _newUnique, newProperties, newChildren) =>
      let oldChild = nodeAt(idx, elems)
      let newChild = createElementNsOptional(newNamespace, newTagName)
      let _: bool = patchVNodesOnElems_Properties(
        callbacks,
        newChild,
        mapEmptyProps(newProperties),
        newProperties,
      )
      let newChildNode = Webapi.Dom.Element.asNode(newChild)
      let childChildren = Webapi.Dom.Node.childNodes(newChildNode)
      let () = patchVNodesOnElems(callbacks, newChildNode, childChildren, 0, list{}, newChildren)
      let _attachedChild = Vdom2.insertBefore(elem, ~new_=newChildNode, ~before=oldChild)
      let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child=oldChild)
    | _ => failwith("Node replacement should never be passed anything but a node itself")
    }

and patchVNodesOnElems_CreateElement = (callbacks: ref<applicationCallbacks<'msg>>): (
  t<'msg> => Dom.node
) =>
  x =>
    switch x {
    | CommentNode(s) =>
      Webapi.Dom.Document.createComment(Webapi.Dom.document, s) |> Webapi.Dom.Comment.asNode
    | Text(text) =>
      Webapi.Dom.Document.createTextNode(Webapi.Dom.document, text) |> Webapi.Dom.Text.asNode
    | @implicit_arity
      Node(newNamespace, newTagName, _newKey, _unique, newProperties, newChildren) =>
      let newChild = createElementNsOptional(newNamespace, newTagName)
      @ocaml.warning("-8")
      let true = patchVNodesOnElems_Properties(
        callbacks,
        newChild,
        mapEmptyProps(newProperties),
        newProperties,
      )
      let newChildNode = Webapi.Dom.Element.asNode(newChild)
      let childChildren = Webapi.Dom.Node.childNodes(newChildNode)
      let () = patchVNodesOnElems(callbacks, newChildNode, childChildren, 0, list{}, newChildren)
      newChildNode
    | @implicit_arity LazyGen(_newKey, newGen, newCache) =>
      let vdom = newGen()
      let () = newCache := vdom
      patchVNodesOnElems_CreateElement(callbacks, vdom)
    | @implicit_arity Tagger(tagger, vdom) =>
      patchVNodesOnElems_CreateElement(tagger(callbacks), vdom)
    }

and patchVNodesOnElems_MutateNode = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.node,
  elems: Dom.nodeList,
  idx: int,
  oldNode: t<'msg>,
  newNode: t<'msg>,
): unit =>
  switch (oldNode, newNode) {
  | (
      @implicit_arity
      Node(_oldNamespace, oldTagName, _oldKey, oldUnique, oldProperties, oldChildren) as _oldNode,
      @implicit_arity
      Node(_newNamespace, newTagName, _newKey, newUnique, newProperties, newChildren) as newNode,
    ) =>
    if oldUnique != newUnique || oldTagName != newTagName {
      patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode)
    } else {
      let child = nodeAt(idx, elems)
      switch Webapi.Dom.Element.ofNode(child) {
      | None => failwith("Expected element in patchVNodesOnElems_MutateNode")
      | Some(childElement) =>
        let childChildren = Webapi.Dom.Node.childNodes(child)
        let () = if (
          patchVNodesOnElems_Properties(callbacks, childElement, oldProperties, newProperties)
        ) {
          ()
        } else {
          let () = Js.log(
            "VDom:  Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure.  This is a massive inefficiency until this issue is resolved.",
          )
          patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode)
        }
        patchVNodesOnElems(callbacks, child, childChildren, 0, oldChildren, newChildren)
      }
    }
  | _ => failwith("Non-node passed to patchVNodesOnElems_MutateNode")
  }

and patchVNodesOnElems = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.node,
  elems: Dom.nodeList,
  idx: int,
  oldVNodes: list<t<'msg>>,
  newVNodes: list<t<'msg>>,
): unit =>
  @ocaml.warning("-4")
  switch (oldVNodes, newVNodes) {
  | (list{@implicit_arity Tagger(_oldTagger, oldVdom), ...oldRest}, _) =>
    patchVNodesOnElems(callbacks, elem, elems, idx, list{oldVdom, ...oldRest}, newVNodes)
  | (list{oldNode, ...oldRest}, list{@implicit_arity Tagger(newTagger, newVdom), ...newRest}) =>
    let () = patchVNodesOnElems(
      newTagger(callbacks),
      elem,
      elems,
      idx,
      list{oldNode},
      list{newVdom},
    )
    patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
  | (list{}, list{}) => ()
  | (list{}, list{newNode, ...newRest}) =>
    let newChild = patchVNodesOnElems_CreateElement(callbacks, newNode)
    let _attachedChild = Webapi.Dom.Node.appendChild(elem, ~child=newChild)
    patchVNodesOnElems(callbacks, elem, elems, idx + 1, list{}, newRest)
  | (list{_oldVnode, ...oldRest}, list{}) =>
    let child = nodeAt(idx, elems)
    let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child)
    patchVNodesOnElems(callbacks, elem, elems, idx, oldRest, list{})
  | (list{CommentNode(oldS), ...oldRest}, list{CommentNode(newS), ...newRest}) if oldS == newS =>
    patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
  | (list{Text(oldText), ...oldRest}, list{Text(newText), ...newRest}) =>
    let () = if oldText == newText {
      ()
    } else {
      let child = nodeAt(idx, elems)
      Webapi.Dom.Node.setNodeValue(child, Js.Null.return(newText))
    }
    patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
  | (
      list{@implicit_arity LazyGen(oldKey, _oldGen, oldCache), ...oldRest},
      list{@implicit_arity LazyGen(newKey, newGen, newCache), ...newRest},
    ) =>
    if oldKey == newKey {
      let () = newCache := oldCache.contents
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
    } else {
      switch (oldRest, newRest) {
      | (
          list{@implicit_arity LazyGen(olderKey, _olderGen, _olderCache), ...olderRest},
          list{@implicit_arity LazyGen(newerKey, _newerGen, _newerCache), ...newerRest},
        ) if olderKey == newKey && oldKey == newerKey =>
        let firstChild = nodeAt(idx, elems)
        let secondChild = nodeAt(idx + 1, elems)
        let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child=secondChild)
        let _attachedChild = Vdom2.insertBefore(elem, ~new_=secondChild, ~before=firstChild)
        patchVNodesOnElems(callbacks, elem, elems, idx + 2, olderRest, newerRest)
      | (list{@implicit_arity LazyGen(olderKey, _olderGen, olderCache), ...olderRest}, _)
        if olderKey == newKey =>
        let oldChild = nodeAt(idx, elems)
        let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child=oldChild)
        let oldVdom = olderCache.contents
        let () = newCache := oldVdom
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, olderRest, newRest)
      | (_, list{@implicit_arity LazyGen(newerKey, _newerGen, _newerCache), ..._newerRest})
        if newerKey == oldKey =>
        let oldChild = nodeAt(idx, elems)
        let newVdom = newGen()
        let () = newCache := newVdom
        let newChild = patchVNodesOnElems_CreateElement(callbacks, newVdom)
        let _attachedChild = Vdom2.insertBefore(elem, ~new_=newChild, ~before=oldChild)
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldVNodes, newRest)
      | _ =>
        let oldVdom = oldCache.contents
        let newVdom = newGen()
        let () = newCache := newVdom
        patchVNodesOnElems(
          callbacks,
          elem,
          elems,
          idx,
          list{oldVdom, ...oldRest},
          list{newVdom, ...newRest},
        )
      }
    }
  | (
      list{
        @implicit_arity
        Node(oldNamespace, oldTagName, oldKey, _oldUnique, _oldProperties, _oldChildren) as oldNode,
        ...oldRest,
      },
      list{
        @implicit_arity
        Node(newNamespace, newTagName, newKey, _newUnique, _newProperties, _newChildren) as newNode,
        ...newRest,
      },
    ) =>
    if oldKey == newKey && oldKey != "" {
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
    } else if oldKey == "" || newKey == "" {
      let () = patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode)
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
    } else {
      switch (oldRest, newRest) {
      | (
          list{
            @implicit_arity
            Node(
              olderNamespace,
              olderTagName,
              olderKey,
              _olderUnique,
              _olderProperties,
              _olderChildren,
            ),
            ...olderRest,
          },
          list{
            @implicit_arity
            Node(
              newerNamespace,
              newerTagName,
              newerKey,
              _newerUnique,
              _newerProperties,
              _newerChildren,
            ),
            ...newerRest,
          },
        )
        if olderNamespace == newNamespace &&
          (olderTagName == newTagName &&
          (olderKey == newKey &&
            (oldNamespace == newerNamespace &&
            (oldTagName == newerTagName && oldKey == newerKey)))) =>
        let firstChild = nodeAt(idx, elems)
        let secondChild = nodeAt(idx + 1, elems)
        let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child=secondChild)
        let _attachedChild = Vdom2.insertBefore(elem, ~new_=secondChild, ~before=firstChild)
        patchVNodesOnElems(callbacks, elem, elems, idx + 2, olderRest, newerRest)
      | (
          list{
            @implicit_arity
            Node(
              olderNamespace,
              olderTagName,
              olderKey,
              _olderUnique,
              _olderProperties,
              _olderChildren,
            ),
            ...olderRest,
          },
          _,
        ) if olderNamespace == newNamespace && (olderTagName == newTagName && olderKey == newKey) =>
        let oldChild = nodeAt(idx, elems)
        let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child=oldChild)
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, olderRest, newRest)
      | (
          _,
          list{
            @implicit_arity
            Node(
              newerNamespace,
              newerTagName,
              newerKey,
              _newerUnique,
              _newerProperties,
              _newerChildren,
            ),
            ..._newerRest,
          },
        ) if oldNamespace == newerNamespace && (oldTagName == newerTagName && oldKey == newerKey) =>
        let oldChild = nodeAt(idx, elems)
        let newChild = patchVNodesOnElems_CreateElement(callbacks, newNode)
        let _attachedChild = Vdom2.insertBefore(elem, ~new_=newChild, ~before=oldChild)
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldVNodes, newRest)
      | _ =>
        let () = patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode)
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
      }
    }
  | (list{_oldVnode, ...oldRest}, list{newNode, ...newRest}) =>
    let oldChild = nodeAt(idx, elems)
    let newChild = patchVNodesOnElems_CreateElement(callbacks, newNode)
    let _attachedChild = Vdom2.insertBefore(elem, ~new_=newChild, ~before=oldChild)
    let _removedChild = Webapi.Dom.Node.removeChild(elem, ~child=oldChild)
    patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
  }

let patchVNodesIntoElement = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.node,
  oldVNodes: list<t<'msg>>,
  newVNodes: list<t<'msg>>,
): list<t<'msg>> => {
  let elems = Webapi.Dom.Node.childNodes(elem)
  let () = patchVNodesOnElems(callbacks, elem, elems, 0, oldVNodes, newVNodes)
  newVNodes
}

let patchVNodeIntoElement = (
  callbacks: ref<applicationCallbacks<'msg>>,
  elem: Dom.node,
  oldVNode: t<'msg>,
  newVNode: t<'msg>,
): list<t<'msg>> => patchVNodesIntoElement(callbacks, elem, list{oldVNode}, list{newVNode})

let wrapCallbacks_On:
  type a b. (a => b, systemMessage<a>) => systemMessage<b> =
  (func, x) =>
    switch x {
    | Render => Render
    | AddRenderMsg(msg) => AddRenderMsg(func(msg))
    | RemoveRenderMsg(msg) => RemoveRenderMsg(func(msg))
    }

let wrapCallbacks:
  type a b. (a => b, ref<applicationCallbacks<b>>) => ref<applicationCallbacks<a>> =
  (func, callbacks) =>
    Obj.magic(
      ref,
      {
        enqueue: (msg: a) => {
          let new_msg = func(msg)
          callbacks.contents.enqueue(new_msg)
        },
        on: smsg => {
          let new_smsg = wrapCallbacks_On(func, smsg)
          callbacks.contents.on(new_smsg)
        },
      },
    )

let map: ('a => 'b, t<'a>) => t<'b> = (
  (func, vdom) => {
    let tagger = wrapCallbacks(func)
    @implicit_arity Tagger(Obj.magic(tagger), Obj.magic(vdom))
  }: ('a => 'b, t<'a>) => t<'b>
)
