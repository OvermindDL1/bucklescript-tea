type t('msg) =
  | NoSub: t(_)
  | Batch(list(t('msg))): t('msg)
  | Registration(
      string,
      (ref(Vdom.applicationCallbacks('msg)), unit) => unit,
      ref(option(unit => unit)),
    )
    : t('msg)
  | Mapper(
      ref(Vdom.applicationCallbacks('msg)) =>
      ref(Vdom.applicationCallbacks('msgB)),
      t('msgB),
    )
    : t('msg);

type applicationCallbacks('msg) = Vdom.applicationCallbacks('msg);

let none = NoSub;

let batch = subs => Batch(subs);

let registration = (key, enableCall) =>
  [@implicit_arity]
  Registration(key, callbacks => enableCall(callbacks^), ref(None));

let map = (msgMapper, sub) => {
  let func = callbacks => Vdom.wrapCallbacks(msgMapper, callbacks);
  [@implicit_arity] Mapper(func, sub);
};

let mapFunc = (func, sub) => [@implicit_arity] Mapper(func, sub);

let rec run:
  type msgOld msgNew.
    (
      ref(Vdom.applicationCallbacks(msgOld)),
      ref(Vdom.applicationCallbacks(msgNew)),
      t(msgOld),
      t(msgNew)
    ) =>
    t(msgNew) =
  (oldCallbacks, newCallbacks, oldSub, newSub) => {
    let rec enable:
      type msg. (ref(Vdom.applicationCallbacks(msg)), t(msg)) => unit =
      callbacks =>
        fun
        | NoSub => ()
        | Batch([]) => ()
        | Batch(subs) => List.iter(enable(callbacks), subs)
        | [@implicit_arity] Mapper(mapper, sub) => {
            let subCallbacks = mapper(callbacks);
            enable(subCallbacks, sub);
          }
        | [@implicit_arity] Registration(_key, enCB, diCB) =>
          diCB := Some(enCB(callbacks));

    let rec disable:
      type msg. (ref(Vdom.applicationCallbacks(msg)), t(msg)) => unit =
      callbacks =>
        fun
        | NoSub => ()
        | Batch([]) => ()
        | Batch(subs) => List.iter(disable(callbacks), subs)
        | [@implicit_arity] Mapper(mapper, sub) => {
            let subCallbacks = mapper(callbacks);
            disable(subCallbacks, sub);
          }
        | [@implicit_arity] Registration(_key, _enCB, diCB) =>
          switch (diCB^) {
          | None => ()
          | Some(cb) =>
            let () = diCB := None;
            cb();
          };

    [@ocaml.warning "-4"]
    (
      switch (oldSub, newSub) {
      | (NoSub, NoSub) => newSub
      | (
          [@implicit_arity] Registration(oldKey, _oldEnCB, oldDiCB),
          [@implicit_arity] Registration(newKey, _newEnCB, newDiCB),
        )
          when oldKey == newKey =>
        let () = newDiCB := oldDiCB^;
        newSub;
      | (
          [@implicit_arity] Mapper(oldMapper, oldSubSub),
          [@implicit_arity] Mapper(newMapper, newSubSub),
        ) =>
        let olderCallbacks = oldMapper(oldCallbacks); /* Resolve the type checker */
        let newerCallbacks = newMapper(newCallbacks);
        let _newerSubSub =
          run(olderCallbacks, newerCallbacks, oldSubSub, newSubSub);
        newSub;
      | (Batch(oldSubs), Batch(newSubs)) =>
        let rec aux = (oldList, newList) =>
          switch (oldList, newList) {
          | ([], []) => ()
          | ([], [newSubSub, ...newRest]) =>
            let () = enable(newCallbacks, newSubSub);
            aux([], newRest);
          | ([oldSubSub, ...oldRest], []) =>
            let () = disable(oldCallbacks, oldSubSub);
            aux(oldRest, []);
          | ([oldSubSub, ...oldRest], [newSubSub, ...newRest]) =>
            let _newerSubSub =
              run(oldCallbacks, newCallbacks, oldSubSub, newSubSub);
            aux(oldRest, newRest);
          };
        let () = aux(oldSubs, newSubs);
        newSub;
      | (oldS, newS) =>
        let () = disable(oldCallbacks, oldS);
        let () = enable(newCallbacks, newS);
        newSub;
      }
    );
  };
