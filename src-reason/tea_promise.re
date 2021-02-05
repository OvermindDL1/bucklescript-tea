let cmd = (promise, tagger) =>
  Vdom.(
    Tea_cmd.call(
      fun
      | callbacks => {
          let _ =
            promise
            |> Js.Promise.then_(
                 fun
                 | res =>
                   switch (tagger(res)) {
                   | Some(msg) =>
                     let () = callbacks^.enqueue(msg);
                     Js.Promise.resolve();
                   | None => Js.Promise.resolve()
                   },
               );

          ();
        },
    )
  );

let result = (promise, msg) =>
  Vdom.(
    Tea_cmd.call(
      fun
      | callbacks => {
          let enq = result => callbacks^.enqueue(msg(result));

          let _ =
            promise
            |> Js.Promise.then_(
                 fun
                 | res => {
                     let resolve = enq(Tea_result.Ok(res));
                     Js.Promise.resolve(resolve);
                   },
               )
            |> Js.Promise.catch(
                 fun
                 | err => {
                     let err_to_string = err => {j|$err|j};
                     let reject = enq(Tea_result.Error(err_to_string(err)));
                     Js.Promise.resolve(reject);
                   },
               );

          ();
        },
    )
  );

let toTask = promise =>
  Tea_task.nativeBinding(cb =>
    let enqRes = (result, _ev) => cb(result);
    let enqResOk = result => enqRes(Tea_result.Ok(result), ());
    let enqResError = result => enqRes(Tea_result.Error(result), ());
    promise
    |> Js.Promise.then_(
         fun
         | res => {
             let resolve = enqResOk(res);
             Js.Promise.resolve(resolve);
           },
       )
    |> Js.Promise.catch(
         fun
         | err => {
             let reject = enqResError(err);
             Js.Promise.resolve(reject);
           },
       )
    |> ignore;
  );
