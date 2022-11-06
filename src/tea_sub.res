type rec t<'msg> =
  | NoSub: t<_>
  | Batch(list<t<'msg>>): t<'msg>
  | Registration(
      string,
      (ref<Vdom.applicationCallbacks<'msg>>, unit) => unit,
      ref<option<unit => unit>>,
    ): t<'msg>
  | Mapper(
      ref<Vdom.applicationCallbacks<'msg>> => ref<Vdom.applicationCallbacks<'msgB>>,
      t<'msgB>,
    ): t<'msg>

type applicationCallbacks<'msg> = Vdom.applicationCallbacks<'msg>

let none = NoSub

let batch = subs => Batch(subs)

let registration = (key, enableCall) => Registration(
  key,
  callbacks => enableCall(callbacks.contents),
  ref(None),
)

let map = (msgMapper, sub) => {
  let func = callbacks => Vdom.wrapCallbacks(msgMapper, callbacks)
  Mapper(func, sub)
}

let mapFunc = (func, sub) => Mapper(func, sub)

let rec run:
  type msgOld msgNew. (
    ref<Vdom.applicationCallbacks<msgOld>>,
    ref<Vdom.applicationCallbacks<msgNew>>,
    t<msgOld>,
    t<msgNew>,
  ) => t<msgNew> =
  (oldCallbacks, newCallbacks, oldSub, newSub) => {
    let rec enable:
      type msg. (ref<Vdom.applicationCallbacks<msg>>, t<msg>) => unit =
      (callbacks, x) =>
        switch x {
        | NoSub => ()
        | Batch(list{}) => ()
        | Batch(subs) => List.iter(enable(callbacks), subs)
        | Mapper(mapper, sub) =>
          let subCallbacks = mapper(callbacks)
          enable(subCallbacks, sub)
        | Registration(_key, enCB, diCB) => diCB := Some(enCB(callbacks))
        }
    let rec disable:
      type msg. (ref<Vdom.applicationCallbacks<msg>>, t<msg>) => unit =
      (callbacks, x) =>
        switch x {
        | NoSub => ()
        | Batch(list{}) => ()
        | Batch(subs) => List.iter(disable(callbacks), subs)
        | Mapper(mapper, sub) =>
          let subCallbacks = mapper(callbacks)
          disable(subCallbacks, sub)
        | Registration(_key, _enCB, diCB) =>
          switch diCB.contents {
          | None => ()
          | Some(cb) =>
            let () = diCB := None
            cb()
          }
        }

    @ocaml.warning("-4")
    switch (oldSub, newSub) {
    | (NoSub, NoSub) => newSub
    | (Registration(oldKey, _oldEnCB, oldDiCB), Registration(newKey, _newEnCB, newDiCB))
      if oldKey == newKey =>
      let () = newDiCB := oldDiCB.contents
      newSub
    | (Mapper(oldMapper, oldSubSub), Mapper(newMapper, newSubSub)) =>
      let olderCallbacks = oldMapper(oldCallbacks)
      let newerCallbacks = newMapper(newCallbacks)
      let _newerSubSub = run(olderCallbacks, newerCallbacks, oldSubSub, newSubSub)
      newSub
    | (Batch(oldSubs), Batch(newSubs)) =>
      let rec aux = (oldList, newList) =>
        switch (oldList, newList) {
        | (list{}, list{}) => ()
        | (list{}, list{newSubSub, ...newRest}) =>
          let () = enable(newCallbacks, newSubSub)
          aux(list{}, newRest)
        | (list{oldSubSub, ...oldRest}, list{}) =>
          let () = disable(oldCallbacks, oldSubSub)
          aux(oldRest, list{})
        | (list{oldSubSub, ...oldRest}, list{newSubSub, ...newRest}) =>
          let _newerSubSub = run(oldCallbacks, newCallbacks, oldSubSub, newSubSub)
          aux(oldRest, newRest)
        }
      let () = aux(oldSubs, newSubs)
      newSub
    | (oldS, newS) =>
      let () = disable(oldCallbacks, oldS)
      let () = enable(newCallbacks, newS)
      newSub
    }
  }
