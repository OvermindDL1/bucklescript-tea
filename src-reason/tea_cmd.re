type applicationCallbacks('msg) = Vdom.applicationCallbacks('msg);

type t('msg) =
  | NoCmd: t(_)
  | Mapper(
      ref(Vdom.applicationCallbacks('msg)) =>
      ref(Vdom.applicationCallbacks('msgB)),
      t('msgB),
    )
    : t('msg)
  | Batch(list(t('msg))): t('msg)
  | EnqueueCall(ref(applicationCallbacks('msg)) => unit): t('msg);

let none = NoCmd;

let batch = cmds => Batch(cmds);

let call = call => EnqueueCall(call);

let fnMsg = fnMsg =>
  Vdom.(EnqueueCall(callbacks => callbacks^.enqueue(fnMsg())));

let msg = msg => Vdom.(EnqueueCall(callbacks => callbacks^.enqueue(msg)));

let rec run: type msg. (ref(applicationCallbacks(msg)), t(msg)) => unit =
  callbacks =>
    fun
    | NoCmd => ()
    | [@implicit_arity] Mapper(mapper, cmd) => {
        let subCallbacks = mapper(callbacks);
        run(subCallbacks, cmd);
      }
    | Batch(cmds) =>
      List.fold_left(((), cmd) => run(callbacks, cmd), (), cmds)
    | EnqueueCall(cb) =>
      /* let () = Js.log ("Cmd.run", "enqueue", cb) in */
      cb(callbacks);

/* let wrapCallbacks func callbacks = */
/*   let open Vdom in */
/*   ref */
/*     { enqueue = (fun msg -> !callbacks.enqueue (func msg)) */
/*     } */

let map: type a b. (a => b, t(a)) => t(b) =
  (func, cmd) => {
    let mapper = Vdom.wrapCallbacks(func);
    [@implicit_arity] Mapper(mapper, cmd);
  };
