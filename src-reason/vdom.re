type systemMessage('msg) =
  | Render
  | AddRenderMsg('msg)
  | RemoveRenderMsg('msg);
type applicationCallbacks('msg) = {
  enqueue: 'msg => unit,
  on: systemMessage('msg) => unit,
};
type eventHandler('msg) =
  | EventHandlerCallback(string, Web.Node.event => option('msg))
  | EventHandlerMsg('msg);
type eventCache('msg) = {
  handler: Web.Node.event_cb,
  cb: ref(Web.Node.event => option('msg)),
};
type property('msg) =
  | NoProp
  | RawProp(string, string)
  | Attribute(string, string, string)
  | Data(string, string)
  | Event(string, eventHandler('msg), ref(option(eventCache('msg))))
  | Style(list((string, string)));
type properties('msg) = list(property('msg));
type t('msg) =
  | CommentNode(string)
  | Text(string)
  | Node(string, string, string, string, properties('msg), list(t('msg)))
  | LazyGen(string, unit => t('msg), ref(t('msg)))
  | Tagger(
      ref(applicationCallbacks('msg)) => ref(applicationCallbacks('msg)),
      t('msg),
    );
let noNode: t('msg) = (CommentNode(""): t('msg));
let comment = (s: string): t('msg) => CommentNode(s);
let text = (s: string): t('msg) => Text(s);
let fullnode =
    (
      namespace: string,
      tagName: string,
      key: string,
      unique: string,
      props: properties('msg),
      vdoms: list(t('msg)),
    )
    : t('msg) =>
  [@implicit_arity] Node(namespace, tagName, key, unique, props, vdoms);
let node =
    (
      ~namespace: string="",
      tagName: string,
      ~key: string="",
      ~unique: string="",
      props: properties('msg),
      vdoms: list(t('msg)),
    )
    : t('msg) =>
  fullnode(namespace, tagName, key, unique, props, vdoms);
let lazyGen = (key: string, fn: unit => t('msg)): t('msg) =>
  [@implicit_arity] LazyGen(key, fn, ref(noNode));
let noProp: property('msg) = (NoProp: property('msg));
let prop = (key: string, value: string): property('msg) =>
  [@implicit_arity] RawProp(key, value);
let onCB =
    (name: string, key: string, cb: Web.Node.event => option('msg))
    : property('msg) =>
  [@implicit_arity]
  Event(name, [@implicit_arity] EventHandlerCallback(key, cb), ref(None));
let onMsg = (name: string, msg: 'msg): property('msg) =>
  [@implicit_arity] Event(name, EventHandlerMsg(msg), ref(None));
let attribute =
    (namespace: string, key: string, value: string): property('msg) =>
  [@implicit_arity] Attribute(namespace, key, value);
let data = (key: string, value: string): property('msg) =>
  [@implicit_arity] Data(key, value);
let style = (key: string, value: string): property('msg) =>
  Style([(key, value)]);
let styles = (s): property('msg) => Style(s);
let rec renderToHtmlString: t('msg) => string = (
  fun
  | CommentNode(s) => "<!-- " ++ s ++ " -->"
  | Text(s) => s
  | [@implicit_arity] Node(namespace, tagName, _key, _unique, props, vdoms) => {
      let renderProp = (
        fun
        | NoProp => ""
        | [@implicit_arity] RawProp(k, v) =>
          String.concat("", [" ", k, "=\"", v, "\""])
        | [@implicit_arity] Attribute(_namespace, k, v) =>
          String.concat("", [" ", k, "=\"", v, "\""])
        | [@implicit_arity] Data(k, v) =>
          String.concat("", [" data-", k, "=\"", v, "\""])
        | [@implicit_arity] Event(_, _, _) => ""
        | Style(s) =>
          String.concat(
            "",
            [
              " style=\"",
              String.concat(
                ";",
                List.map(
                  ((k, v)) => String.concat("", [k, ":", v, ";"]),
                  s,
                ),
              ),
              "\"",
            ],
          )
      );
      String.concat(
        "",
        [
          "<",
          tagName,
          if (namespace == "") {
            "";
          } else {
            " xmlns=\"" ++ namespace ++ "\"";
          },
          String.concat("", List.map(p => renderProp(p), props)),
          ">",
          String.concat("", List.map(v => renderToHtmlString(v), vdoms)),
          "</",
          tagName,
          ">",
        ],
      );
    }
  | [@implicit_arity] LazyGen(_key, gen, _cache) => {
      let vdom = gen();
      renderToHtmlString(vdom);
    }
  | [@implicit_arity] Tagger(_tagger, vdom) => renderToHtmlString(vdom):
    t('msg) => string
);
let emptyEventHandler: Web.Node.event_cb = ((. _ev) => (): Web.Node.event_cb);
let emptyEventCB = (_ev): option(Web.Node.event_cb) => None;
let eventHandler =
    (
      callbacks: ref(applicationCallbacks('msg)),
      cb: ref(Web.Node.event => option('msg)),
    )
    : Web.Node.event_cb =>
  (. ev) =>
    switch (cb^(ev)) {
    | None => ()
    | Some(msg) => callbacks^.enqueue(msg)
    };
let eventHandler_GetCB: (eventHandler('msg), Web.Node.event) => option('msg) = (
  fun
  | [@implicit_arity] EventHandlerCallback(_, cb) => cb
  | EventHandlerMsg(msg) => (_ev => Some(msg)):
    (eventHandler('msg), Web.Node.event) => option('msg)
);
let compareEventHandlerTypes =
    (left: eventHandler('msg)): (eventHandler('msg) => bool) =>
  fun
  | [@implicit_arity] EventHandlerCallback(cb, _) =>
    switch (left) {
    | [@implicit_arity] EventHandlerCallback(lcb, _) when cb == lcb => true
    | _ => false
    }
  | EventHandlerMsg(msg) =>
    switch (left) {
    | EventHandlerMsg(lmsg) when msg == lmsg => true
    | _ => false
    };
let eventHandler_Register =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      name: string,
      handlerType: eventHandler('msg),
    )
    : option(eventCache('msg)) => {
  let cb = ref(eventHandler_GetCB(handlerType));
  let handler = eventHandler(callbacks, cb);
  let () = Web.Node.addEventListener(elem, name, handler, false);
  Some({handler, cb});
};
let eventHandler_Unregister =
    (elem: Web.Node.t, name: string)
    : (option(eventCache('msg)) => option(eventCache('msg))) =>
  fun
  | None => None
  | Some(cache) => {
      let () = Web.Node.removeEventListener(elem, name, cache.handler, false);
      None;
    };
let eventHandler_Mutate =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      oldName: string,
      newName: string,
      oldHandlerType: eventHandler('msg),
      newHandlerType: eventHandler('msg),
      oldCache: ref(option(eventCache('msg))),
      newCache: ref(option(eventCache('msg))),
    )
    : unit =>
  switch (oldCache^) {
  | None =>
    newCache :=
      eventHandler_Register(callbacks, elem, newName, newHandlerType)
  | Some(oldcache) =>
    if (oldName == newName) {
      let () = newCache := oldCache^;
      if (compareEventHandlerTypes(oldHandlerType, newHandlerType)) {
        ();
      } else {
        let cb = eventHandler_GetCB(newHandlerType);
        let () = oldcache.cb := cb;
        ();
      };
    } else {
      let () = oldCache := eventHandler_Unregister(elem, oldName, oldCache^);
      let () =
        newCache :=
          eventHandler_Register(callbacks, elem, newName, newHandlerType);
      ();
    }
  };
let patchVNodesOnElems_PropertiesApply_Add =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      _idx: int,
    )
    : (property('msg) => unit) =>
  fun
  | NoProp => ()
  | [@implicit_arity] RawProp(k, v) => Web.Node.setProp(elem, k, v)
  | [@implicit_arity] Attribute(namespace, k, v) =>
    Web.Node.setAttributeNsOptional(elem, namespace, k, v)
  | [@implicit_arity] Data(k, v) => {
      Js.log(("TODO:  Add Data Unhandled", k, v));
      failwith("TODO:  Add Data Unhandled");
    }
  | [@implicit_arity] Event(name, handlerType, cache) =>
    cache := eventHandler_Register(callbacks, elem, name, handlerType)
  | Style(s) =>
    List.fold_left(
      ((), (k, v)) =>
        Web.Node.setStyleProperty(elem, k, Js.Null.return(v)),
      (),
      s,
    );
let patchVNodesOnElems_PropertiesApply_Remove =
    (
      _callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      _idx: int,
    )
    : (property('msg) => unit) =>
  fun
  | NoProp => ()
  | [@implicit_arity] RawProp(k, _v) =>
    Web.Node.setProp(elem, k, Js.Undefined.empty)
  | [@implicit_arity] Attribute(namespace, k, _v) =>
    Web.Node.removeAttributeNsOptional(elem, namespace, k)
  | [@implicit_arity] Data(k, v) => {
      Js.log(("TODO:  Remove Data Unhandled", k, v));
      failwith("TODO:  Remove Data Unhandled");
    }
  | [@implicit_arity] Event(name, _, cache) =>
    cache := eventHandler_Unregister(elem, name, cache^)
  | Style(s) =>
    List.fold_left(
      ((), (k, _v)) => Web.Node.setStyleProperty(elem, k, Js.Null.empty),
      (),
      s,
    );
let patchVNodesOnElems_PropertiesApply_RemoveAdd =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      idx: int,
      oldProp: property('msg),
      newProp: property('msg),
    )
    : unit => {
  let () =
    patchVNodesOnElems_PropertiesApply_Remove(callbacks, elem, idx, oldProp);
  let () =
    patchVNodesOnElems_PropertiesApply_Add(callbacks, elem, idx, newProp);
  ();
};
let patchVNodesOnElems_PropertiesApply_Mutate =
    (
      _callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      _idx: int,
      oldProp: property('msg),
    )
    : (property('msg) => unit) =>
  fun
  | NoProp as _newProp =>
    failwith(
      "This should never be called as all entries through NoProp are gated.",
    )
  | [@implicit_arity] RawProp(k, v) as _newProp =>
    Web.Node.setProp(elem, k, v)
  | [@implicit_arity] Attribute(namespace, k, v) as _newProp =>
    Web.Node.setAttributeNsOptional(elem, namespace, k, v)
  | [@implicit_arity] Data(k, v) as _newProp => {
      Js.log(("TODO:  Mutate Data Unhandled", k, v));
      failwith("TODO:  Mutate Data Unhandled");
    }
  | [@implicit_arity] Event(_newName, _newHandlerType, _newCache) as _newProp =>
    failwith("This will never be called because it is gated")
  | Style(s) as _newProp =>
    [@ocaml.warning "-4"]
    (
      switch (oldProp) {
      | Style(oldS) =>
        List.fold_left2(
          ((), (ok, ov), (nk, nv)) =>
            if (ok == nk) {
              if (ov == nv) {
                ();
              } else {
                Web.Node.setStyleProperty(elem, nk, Js.Null.return(nv));
              };
            } else {
              let () = Web.Node.setStyleProperty(elem, ok, Js.Null.empty);
              Web.Node.setStyleProperty(elem, nk, Js.Null.return(nv));
            },
          (),
          oldS,
          s,
        )
      | _ =>
        failwith(
          "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!",
        )
      }
    );
let rec patchVNodesOnElems_PropertiesApply =
        (
          callbacks: ref(applicationCallbacks('msg)),
          elem: Web.Node.t,
          idx: int,
          oldProperties: list(property('msg)),
          newProperties: list(property('msg)),
        )
        : bool =>
  [@ocaml.warning "-4"]
  (
    switch (oldProperties, newProperties) {
    | ([], []) => true
    | ([], [_newProp, ..._newRest]) => false
    | ([_oldProp, ..._oldRest], []) => false
    | ([NoProp, ...oldRest], [NoProp, ...newRest]) =>
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      )
    | (
        [[@implicit_arity] RawProp(oldK, oldV) as oldProp, ...oldRest],
        [[@implicit_arity] RawProp(newK, newV) as newProp, ...newRest],
      ) =>
      let () =
        if (oldK == newK && oldV == newV) {
          ();
        } else {
          patchVNodesOnElems_PropertiesApply_Mutate(
            callbacks,
            elem,
            idx,
            oldProp,
            newProp,
          );
        };
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      );
    | (
        [
          [@implicit_arity] Attribute(oldNS, oldK, oldV) as oldProp,
          ...oldRest,
        ],
        [
          [@implicit_arity] Attribute(newNS, newK, newV) as newProp,
          ...newRest,
        ],
      ) =>
      let () =
        if (oldNS == newNS && oldK == newK && oldV == newV) {
          ();
        } else {
          patchVNodesOnElems_PropertiesApply_Mutate(
            callbacks,
            elem,
            idx,
            oldProp,
            newProp,
          );
        };
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      );
    | (
        [[@implicit_arity] Data(oldK, oldV) as oldProp, ...oldRest],
        [[@implicit_arity] Data(newK, newV) as newProp, ...newRest],
      ) =>
      let () =
        if (oldK == newK && oldV == newV) {
          ();
        } else {
          patchVNodesOnElems_PropertiesApply_Mutate(
            callbacks,
            elem,
            idx,
            oldProp,
            newProp,
          );
        };
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      );
    | (
        [
          [@implicit_arity] Event(oldName, oldHandlerType, oldCache) as _oldProp,
          ...oldRest,
        ],
        [
          [@implicit_arity] Event(newName, newHandlerType, newCache) as _newProp,
          ...newRest,
        ],
      ) =>
      let () =
        eventHandler_Mutate(
          callbacks,
          elem,
          oldName,
          newName,
          oldHandlerType,
          newHandlerType,
          oldCache,
          newCache,
        );
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      );
    | (
        [Style(oldS) as oldProp, ...oldRest],
        [Style(newS) as newProp, ...newRest],
      ) =>
      let () =
        if (oldS == newS) {
          ();
        } else {
          patchVNodesOnElems_PropertiesApply_Mutate(
            callbacks,
            elem,
            idx,
            oldProp,
            newProp,
          );
        };
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      );
    | ([oldProp, ...oldRest], [newProp, ...newRest]) =>
      let () =
        patchVNodesOnElems_PropertiesApply_RemoveAdd(
          callbacks,
          elem,
          idx,
          oldProp,
          newProp,
        );
      patchVNodesOnElems_PropertiesApply(
        callbacks,
        elem,
        idx + 1,
        oldRest,
        newRest,
      );
    }
  );
let patchVNodesOnElems_Properties =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      oldProperties: list(property('msg)),
      newProperties: list(property('msg)),
    )
    : bool =>
  patchVNodesOnElems_PropertiesApply(
    callbacks,
    elem,
    0,
    oldProperties,
    newProperties,
  );
let genEmptyProps = (length: int): list(property('msg)) => {
  let rec aux = lst =>
    fun
    | 0 => lst
    | len => aux([noProp, ...lst], len - 1);
  aux([], length);
};
let mapEmptyProps = (props: list(property('msg))): list(property('msg)) =>
  List.map(_ => noProp, props);
let rec patchVNodesOnElems_ReplaceNode =
        (
          callbacks: ref(applicationCallbacks('msg)),
          elem: Web.Node.t,
          elems: array(Web.Node.t),
          idx: int,
        )
        : (t('msg) => unit) =>
  [@ocaml.warning "-4"]
  (
    fun
    | [@implicit_arity]
      Node(
        newNamespace,
        newTagName,
        _newKey,
        _newUnique,
        newProperties,
        newChildren,
      ) => {
        let oldChild = elems[idx];
        let newChild =
          Web.Document.createElementNsOptional(newNamespace, newTagName);
        [@ocaml.warning "-8"]
        let true =
          patchVNodesOnElems_Properties(
            callbacks,
            newChild,
            mapEmptyProps(newProperties),
            newProperties,
          );
        let childChildren = Web.Node.childNodes(newChild);
        let () =
          patchVNodesOnElems(
            callbacks,
            newChild,
            childChildren,
            0,
            [],
            newChildren,
          );
        let _attachedChild = Web.Node.insertBefore(elem, newChild, oldChild);
        let _removedChild = Web.Node.removeChild(elem, oldChild);
        ();
      }
    | _ =>
      failwith(
        "Node replacement should never be passed anything but a node itself",
      )
  )
and patchVNodesOnElems_CreateElement =
    (callbacks: ref(applicationCallbacks('msg))): (t('msg) => Web.Node.t) =>
  fun
  | CommentNode(s) => Web.Document.createComment(s)
  | Text(text) => Web.Document.createTextNode(text)
  | [@implicit_arity]
    Node(
      newNamespace,
      newTagName,
      _newKey,
      _unique,
      newProperties,
      newChildren,
    ) => {
      let newChild =
        Web.Document.createElementNsOptional(newNamespace, newTagName);
      [@ocaml.warning "-8"]
      let true =
        patchVNodesOnElems_Properties(
          callbacks,
          newChild,
          mapEmptyProps(newProperties),
          newProperties,
        );
      let childChildren = Web.Node.childNodes(newChild);
      let () =
        patchVNodesOnElems(
          callbacks,
          newChild,
          childChildren,
          0,
          [],
          newChildren,
        );
      newChild;
    }
  | [@implicit_arity] LazyGen(_newKey, newGen, newCache) => {
      let vdom = newGen();
      let () = newCache := vdom;
      patchVNodesOnElems_CreateElement(callbacks, vdom);
    }
  | [@implicit_arity] Tagger(tagger, vdom) =>
    patchVNodesOnElems_CreateElement(tagger(callbacks), vdom)
and patchVNodesOnElems_MutateNode =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      elems: array(Web.Node.t),
      idx: int,
      oldNode: t('msg),
      newNode: t('msg),
    )
    : unit =>
  switch (oldNode, newNode) {
  | (
      [@implicit_arity]
      Node(
        _oldNamespace,
        oldTagName,
        _oldKey,
        oldUnique,
        oldProperties,
        oldChildren,
      ) as _oldNode,
      [@implicit_arity]
      Node(
        _newNamespace,
        newTagName,
        _newKey,
        newUnique,
        newProperties,
        newChildren,
      ) as newNode,
    ) =>
    if (oldUnique != newUnique || oldTagName != newTagName) {
      patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode);
    } else {
      let child = elems[idx];
      let childChildren = Web.Node.childNodes(child);
      let () =
        if (patchVNodesOnElems_Properties(
              callbacks,
              child,
              oldProperties,
              newProperties,
            )) {
          ();
        } else {
          let () =
            Js.log(
              "VDom:  Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure.  This is a massive inefficiency until this issue is resolved.",
            );
          patchVNodesOnElems_ReplaceNode(
            callbacks,
            elem,
            elems,
            idx,
            newNode,
          );
        };
      patchVNodesOnElems(
        callbacks,
        child,
        childChildren,
        0,
        oldChildren,
        newChildren,
      );
    }
  | _ => failwith("Non-node passed to patchVNodesOnElems_MutateNode")
  }
and patchVNodesOnElems =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      elems: array(Web.Node.t),
      idx: int,
      oldVNodes: list(t('msg)),
      newVNodes: list(t('msg)),
    )
    : unit =>
  [@ocaml.warning "-4"]
  (
    switch (oldVNodes, newVNodes) {
    | ([[@implicit_arity] Tagger(_oldTagger, oldVdom), ...oldRest], _) =>
      patchVNodesOnElems(
        callbacks,
        elem,
        elems,
        idx,
        [oldVdom, ...oldRest],
        newVNodes,
      )
    | (
        [oldNode, ...oldRest],
        [[@implicit_arity] Tagger(newTagger, newVdom), ...newRest],
      ) =>
      let () =
        patchVNodesOnElems(
          newTagger(callbacks),
          elem,
          elems,
          idx,
          [oldNode],
          [newVdom],
        );
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest);
    | ([], []) => ()
    | ([], [newNode, ...newRest]) =>
      let newChild = patchVNodesOnElems_CreateElement(callbacks, newNode);
      let _attachedChild = Web.Node.appendChild(elem, newChild);
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, [], newRest);
    | ([_oldVnode, ...oldRest], []) =>
      let child = elems[idx];
      let _removedChild = Web.Node.removeChild(elem, child);
      patchVNodesOnElems(callbacks, elem, elems, idx, oldRest, []);
    | ([CommentNode(oldS), ...oldRest], [CommentNode(newS), ...newRest])
        when oldS == newS =>
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest)
    | ([Text(oldText), ...oldRest], [Text(newText), ...newRest]) =>
      let () =
        if (oldText == newText) {
          ();
        } else {
          let child = elems[idx];
          Web.Node.set_nodeValue(child, newText);
        };
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest);
    | (
        [[@implicit_arity] LazyGen(oldKey, _oldGen, oldCache), ...oldRest],
        [[@implicit_arity] LazyGen(newKey, newGen, newCache), ...newRest],
      ) =>
      if (oldKey == newKey) {
        let () = newCache := oldCache^;
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest);
      } else {
        switch (oldRest, newRest) {
        | (
            [
              [@implicit_arity] LazyGen(olderKey, _olderGen, _olderCache),
              ...olderRest,
            ],
            [
              [@implicit_arity] LazyGen(newerKey, _newerGen, _newerCache),
              ...newerRest,
            ],
          )
            when olderKey == newKey && oldKey == newerKey =>
          let firstChild = elems[idx];
          let secondChild = elems[idx + 1];
          let _removedChild = Web.Node.removeChild(elem, secondChild);
          let _attachedChild =
            Web.Node.insertBefore(elem, secondChild, firstChild);
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 2,
            olderRest,
            newerRest,
          );
        | (
            [
              [@implicit_arity] LazyGen(olderKey, _olderGen, olderCache),
              ...olderRest,
            ],
            _,
          )
            when olderKey == newKey =>
          let oldChild = elems[idx];
          let _removedChild = Web.Node.removeChild(elem, oldChild);
          let oldVdom = olderCache^;
          let () = newCache := oldVdom;
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 1,
            olderRest,
            newRest,
          );
        | (
            _,
            [
              [@implicit_arity] LazyGen(newerKey, _newerGen, _newerCache),
              ..._newerRest,
            ],
          )
            when newerKey == oldKey =>
          let oldChild = elems[idx];
          let newVdom = newGen();
          let () = newCache := newVdom;
          let newChild = patchVNodesOnElems_CreateElement(callbacks, newVdom);
          let _attachedChild =
            Web.Node.insertBefore(elem, newChild, oldChild);
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 1,
            oldVNodes,
            newRest,
          );
        | _ =>
          let oldVdom = oldCache^;
          let newVdom = newGen();
          let () = newCache := newVdom;
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx,
            [oldVdom, ...oldRest],
            [newVdom, ...newRest],
          );
        };
      }
    | (
        [
          [@implicit_arity]
          Node(
            oldNamespace,
            oldTagName,
            oldKey,
            _oldUnique,
            _oldProperties,
            _oldChildren,
          ) as oldNode,
          ...oldRest,
        ],
        [
          [@implicit_arity]
          Node(
            newNamespace,
            newTagName,
            newKey,
            _newUnique,
            _newProperties,
            _newChildren,
          ) as newNode,
          ...newRest,
        ],
      ) =>
      if (oldKey == newKey && oldKey != "") {
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest);
      } else if (oldKey == "" || newKey == "") {
        let () =
          patchVNodesOnElems_MutateNode(
            callbacks,
            elem,
            elems,
            idx,
            oldNode,
            newNode,
          );
        patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest);
      } else {
        switch (oldRest, newRest) {
        | (
            [
              [@implicit_arity]
              Node(
                olderNamespace,
                olderTagName,
                olderKey,
                _olderUnique,
                _olderProperties,
                _olderChildren,
              ),
              ...olderRest,
            ],
            [
              [@implicit_arity]
              Node(
                newerNamespace,
                newerTagName,
                newerKey,
                _newerUnique,
                _newerProperties,
                _newerChildren,
              ),
              ...newerRest,
            ],
          )
            when
              olderNamespace == newNamespace
              && olderTagName == newTagName
              && olderKey == newKey
              && oldNamespace == newerNamespace
              && oldTagName == newerTagName
              && oldKey == newerKey =>
          let firstChild = elems[idx];
          let secondChild = elems[idx + 1];
          let _removedChild = Web.Node.removeChild(elem, secondChild);
          let _attachedChild =
            Web.Node.insertBefore(elem, secondChild, firstChild);
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 2,
            olderRest,
            newerRest,
          );
        | (
            [
              [@implicit_arity]
              Node(
                olderNamespace,
                olderTagName,
                olderKey,
                _olderUnique,
                _olderProperties,
                _olderChildren,
              ),
              ...olderRest,
            ],
            _,
          )
            when
              olderNamespace == newNamespace
              && olderTagName == newTagName
              && olderKey == newKey =>
          let oldChild = elems[idx];
          let _removedChild = Web.Node.removeChild(elem, oldChild);
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 1,
            olderRest,
            newRest,
          );
        | (
            _,
            [
              [@implicit_arity]
              Node(
                newerNamespace,
                newerTagName,
                newerKey,
                _newerUnique,
                _newerProperties,
                _newerChildren,
              ),
              ..._newerRest,
            ],
          )
            when
              oldNamespace == newerNamespace
              && oldTagName == newerTagName
              && oldKey == newerKey =>
          let oldChild = elems[idx];
          let newChild = patchVNodesOnElems_CreateElement(callbacks, newNode);
          let _attachedChild =
            Web.Node.insertBefore(elem, newChild, oldChild);
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 1,
            oldVNodes,
            newRest,
          );
        | _ =>
          let () =
            patchVNodesOnElems_MutateNode(
              callbacks,
              elem,
              elems,
              idx,
              oldNode,
              newNode,
            );
          patchVNodesOnElems(
            callbacks,
            elem,
            elems,
            idx + 1,
            oldRest,
            newRest,
          );
        };
      }
    | ([_oldVnode, ...oldRest], [newNode, ...newRest]) =>
      let oldChild = elems[idx];
      let newChild = patchVNodesOnElems_CreateElement(callbacks, newNode);
      let _attachedChild = Web.Node.insertBefore(elem, newChild, oldChild);
      let _removedChild = Web.Node.removeChild(elem, oldChild);
      patchVNodesOnElems(callbacks, elem, elems, idx + 1, oldRest, newRest);
    }
  );
let patchVNodesIntoElement =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      oldVNodes: list(t('msg)),
      newVNodes: list(t('msg)),
    )
    : list(t('msg)) => {
  let elems = Web.Node.childNodes(elem);
  let () =
    patchVNodesOnElems(callbacks, elem, elems, 0, oldVNodes, newVNodes);
  newVNodes;
};
let patchVNodeIntoElement =
    (
      callbacks: ref(applicationCallbacks('msg)),
      elem: Web.Node.t,
      oldVNode: t('msg),
      newVNode: t('msg),
    )
    : list(t('msg)) =>
  patchVNodesIntoElement(callbacks, elem, [oldVNode], [newVNode]);
let wrapCallbacks_On:
  type a b. (a => b, systemMessage(a)) => systemMessage(b) =
  func =>
    fun
    | Render => Render
    | AddRenderMsg(msg) => AddRenderMsg(func(msg))
    | RemoveRenderMsg(msg) => RemoveRenderMsg(func(msg));
let wrapCallbacks:
  type a b.
    (a => b, ref(applicationCallbacks(b))) => ref(applicationCallbacks(a)) =
  (func, callbacks) =>
    Obj.magic(
      ref,
      {
        enqueue: (msg: a) => {
          let new_msg = func(msg);
          callbacks^.enqueue(new_msg);
        },
        on: smsg => {
          let new_smsg = wrapCallbacks_On(func, smsg);
          callbacks^.on(new_smsg);
        },
      },
    );
let map: ('a => 'b, t('a)) => t('b) = (
  (func, vdom) => {
    let tagger = wrapCallbacks(func);
    [@implicit_arity] Tagger(Obj.magic(tagger), Obj.magic(vdom));
  }:
    ('a => 'b, t('a)) => t('b)
);
