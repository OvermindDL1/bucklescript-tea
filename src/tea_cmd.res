type applicationCallbacks<'msg> = Vdom.applicationCallbacks<'msg>

type rec t<'msg> =
  | NoCmd: t<_>
  | Mapper(
      ref<Vdom.applicationCallbacks<'msg>> => ref<Vdom.applicationCallbacks<'msgB>>,
      t<'msgB>,
    ): t<'msg>
  | Batch(list<t<'msg>>): t<'msg>
  | EnqueueCall(ref<applicationCallbacks<'msg>> => unit): t<'msg>

let none = NoCmd

let batch = cmds => Batch(cmds)

let call = call => EnqueueCall(call)

let fnMsg = fnMsg => {
  open Vdom
  EnqueueCall(callbacks => callbacks.contents.enqueue(fnMsg()))
}

let msg = msg => {
  open Vdom
  EnqueueCall(callbacks => callbacks.contents.enqueue(msg))
}

let rec run:
  type msg. (ref<applicationCallbacks<msg>>, t<msg>) => unit =
  (callbacks, x) =>
    switch x {
    | NoCmd => ()
    | Mapper(mapper, cmd) =>
      let subCallbacks = mapper(callbacks)
      run(subCallbacks, cmd)
    | Batch(cmds) => List.fold_left(((), cmd) => run(callbacks, cmd), (), cmds)
    | EnqueueCall(cb) => cb(callbacks)
    }

let map:
  type a b. (a => b, t<a>) => t<b> =
  (func, cmd) => {
    let mapper = Vdom.wrapCallbacks(func)
    Mapper(mapper, cmd)
  }
