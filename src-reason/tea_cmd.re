type applicationCallbacks('msg) = Vdom.applicationCallbacks('msg);

type t('msg) =
  | NoCmd
  | Tagger(ref(applicationCallbacks('msg)) => unit)
  | Batch(list(t('msg)))
  | EnqueueCall(ref(applicationCallbacks('msg)) => unit);

let none = NoCmd;

let batch = cmds => Batch(cmds);

let call = call => EnqueueCall(call);

let fnMsg = fnMsg =>
  Vdom.(EnqueueCall(callbacks => callbacks^.enqueue(fnMsg())));

let msg = msg => Vdom.(EnqueueCall(callbacks => callbacks^.enqueue(msg)));

let rec run = callbacks =>
  fun
  | NoCmd => ()
  | Tagger(tagger) => tagger(callbacks)
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
let map: ('a => 'b, t('a)) => t('b) =
  (func, cmd) =>
    Vdom.(Tagger(callbacks => run(wrapCallbacks(func, callbacks), cmd)));
