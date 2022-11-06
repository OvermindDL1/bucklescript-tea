type rec t<'msg> =
  | NoSub: t<'msg>
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

let none: t<'msg>

let batch: list<t<'msg>> => t<'msg>

let registration: (string, (Vdom.applicationCallbacks<'msg>, unit) => unit) => t<'msg>

let map: ('msg => 'b, t<'msg>) => t<'b>

let mapFunc: (ref<Vdom.applicationCallbacks<'msg>> => ref<Vdom.applicationCallbacks<'b>>, t<'b>) => t<'msg,>

let run: (
  ref<Vdom.applicationCallbacks<'msgOld>>,
  ref<Vdom.applicationCallbacks<'msgNew>>,
  t<'msgOld>,
  t<'msgNew>,
) => t<'msgNew>
