type applicationCallbacks<'msg> = Vdom.applicationCallbacks<'msg>

type rec t<'msg> =
  | NoCmd: t<'msg>
  | Mapper(
      ref<Vdom.applicationCallbacks<'msg>> => ref<Vdom.applicationCallbacks<'msgB>>,
      t<'msgB>,
    ): t<'msg>
  | Batch(list<t<'msg>>): t<'msg>
  | EnqueueCall(ref<applicationCallbacks<'msg>> => unit): t<'msg>

let none: t<'msg>

let batch: list<t<'msg>> => t<'msg>

let call: (ref<applicationCallbacks<'msg>> => unit) => t<'msg>

let fnMsg: (unit => 'msg) => t<'msg>

let msg: 'msg => t<'msg>

let run: (ref<applicationCallbacks<'msg>>, t<'msg>) => unit

let map: ('msg => 'b, t<'msg>) => t<'b>

