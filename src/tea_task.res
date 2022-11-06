type never

type rec t<'succeed, 'fail> = Task((result<'succeed, 'fail> => unit) => unit): t<'succeed, 'fail>

let nothing = () => ()

let performOpt = (
  toOptionalMessage: 'value => option<'msg>,
  Task(task): t<'value, never>,
): Tea_cmd.t<'msg> =>
  Tea_cmd.call(callbacks => {
    open Vdom
    let cb = x =>
      switch x {
      | Error(_e) =>
        failwith(
          @reason.raw_literal("ERROR:  Task perfom returned error of never! Should not happen!")
          "ERROR:  Task perfom returned error of never! Should not happen!",
        )
      | Ok(v) =>
        switch toOptionalMessage(v) {
        | None => ()
        | Some(result) => callbacks.contents.enqueue(result)
        }
      }
    task(cb)
  })

let perform = (toMessage: 'value => 'msg, task: t<'value, never>): Tea_cmd.t<'msg> =>
  performOpt(v => Some(toMessage(v)), task)

let attemptOpt = (
  resultToOptionalMessage: result<'succeed, 'fail> => option<'msg>,
  Task(task): t<'succeed, 'fail>,
): Tea_cmd.t<'msg> =>
  Tea_cmd.call(callbacks => {
    open Vdom
    let cb = value =>
      switch resultToOptionalMessage(value) {
      | None => ()
      | Some(result) => callbacks.contents.enqueue(result)
      }
    task(cb)
  })

let attempt = (
  resultToMessage: result<'succeed, 'fail> => 'msg,
  task: t<'succeed, 'fail>,
): Tea_cmd.t<'msg> => attemptOpt(v => Some(resultToMessage(v)), task)

let ignore = task => attemptOpt(_ => None, task)

let succeed = (value: 'v): t<'v, 'e> => Task(cb => cb(Ok(value)))

let fail = (value: 'v): t<'e, 'v> => Task(cb => cb(Error(value)))

let nativeBinding = (func: (result<'succeed, 'fail> => unit) => unit): t<'succeed, 'fail> => Task(
  func,
)

let andThen = (fn, Task(task)) => Task(
  cb =>
    task(x =>
      switch x {
      | Error(_e) as err => cb(err)
      | Ok(v) =>
        let Task(nextTask) = fn(v)
        nextTask(cb)
      }
    ),
)

let onError = (fn, Task(task)) => Task(
  cb =>
    task(x =>
      switch x {
      | Ok(_v) as ok => cb(ok)
      | Error(e) =>
        let Task(newTask) = fn(e)
        newTask(cb)
      }
    ),
)

let fromResult: result<'success, 'failure> => t<'success, 'failure> = x =>
  switch x {
  | Ok(s) => succeed(s)
  | Error(err) => fail(err)
  }

let mapError = (func, task) => task |> onError(e => fail(func(e)))

let toOption = task => task |> andThen(v => succeed(Some(v))) |> onError(_ => succeed(None))

let map = (func, task1) => task1 |> andThen(v1 => succeed(func(v1)))

let map2 = (func, task1, task2) =>
  task1 |> andThen(v1 => task2 |> andThen(v2 => succeed(func(v1, v2))))

let map3 = (func, task1, task2, task3) =>
  task1 |> andThen(v1 => task2 |> andThen(v2 => task3 |> andThen(v3 => succeed(func(v1, v2, v3)))))

let map4 = (func, task1, task2, task3, task4) =>
  task1 |> andThen(v1 =>
    task2 |> andThen(v2 =>
      task3 |> andThen(v3 => task4 |> andThen(v4 => succeed(func(v1, v2, v3, v4))))
    )
  )

let map5 = (func, task1, task2, task3, task4, task5) =>
  task1 |> andThen(v1 =>
    task2 |> andThen(v2 =>
      task3 |> andThen(v3 =>
        task4 |> andThen(v4 => task5 |> andThen(v5 => succeed(func(v1, v2, v3, v4, v5))))
      )
    )
  )

let map6 = (func, task1, task2, task3, task4, task5, task6) =>
  task1 |> andThen(v1 =>
    task2 |> andThen(v2 =>
      task3 |> andThen(v3 =>
        task4 |> andThen(v4 =>
          task5 |> andThen(v5 => task6 |> andThen(v6 => succeed(func(v1, v2, v3, v4, v5, v6))))
        )
      )
    )
  )

let rec sequence = x =>
  switch x {
  | list{} => succeed(list{})
  | list{task, ...remainingTasks} => map2((l, r) => list{l, ...r}, task, sequence(remainingTasks))
  }

let testing_deop = ref(true)

let testing = () => {
  let doTest = (expected, Task(task)) => {
    let testAssert = v =>
      if v == expected {
        Js.log((@reason.raw_literal("Passed:") "Passed:", expected, v))
      } else {
        Js.log((@reason.raw_literal("FAILED:") "FAILED:", expected, v))
      }
    task(testAssert)
  }
  let s = succeed(42)
  let () = doTest(Ok(42), s)
  let f = fail(86)
  let () = doTest(Error(86), f)
  let r = () =>
    if testing_deop.contents {
      succeed(42)
    } else {
      fail(3.14)
    }
  let a1 = succeed(2) |> andThen(n => succeed(n + 2))
  let () = doTest(Ok(4), a1)
  let a2 = succeed(2) |> andThen(n => succeed(string_of_int(n)))
  let () = doTest(Ok(@reason.raw_literal("2") "2"), a2)
  let m1 = map(sqrt, succeed(9.))
  let () = doTest(Ok(3.), m1)
  let m2 = map2(\"+", succeed(9), succeed(3))
  let () = doTest(Ok(12), m2)
  let m3 = map(string_of_int, succeed(9))
  let () = doTest(Ok(@reason.raw_literal("9") "9"), m3)
  let s0 = sequence(list{succeed(1), succeed(2)})
  let () = doTest(Ok(list{1, 2}), s0)
  let s1 = sequence(list{succeed(1), fail(2.7), r()})
  let () = doTest(Error(2.7), s1)
  let e0 =
    fail(@reason.raw_literal("file not found") "file not found") |> onError(_msg => succeed(42))
  let () = doTest(Ok(42), e0)
  let e1 = fail(@reason.raw_literal("file not found") "file not found") |> onError(_msg => fail(42))
  let () = doTest(Error(42), e1)
  let n0 = sequence(list{
    mapError(string_of_int, fail(42)),
    mapError(Js.Float.toString, fail(3.14)),
  })
  let () = doTest(Error(@reason.raw_literal("42") "42"), n0)
  let n1 = sequence(list{
    mapError(string_of_int, succeed(1)),
    mapError(Js.Float.toString, fail(3.14)),
  })
  let () = doTest(Error(@reason.raw_literal("3.14") "3.14"), n1)
  let n2 = sequence(list{
    mapError(string_of_int, succeed(1)),
    mapError(Js.Float.toString, succeed(2)),
  })
  let () = doTest(Ok(list{1, 2}), n2)

  let _c0 = perform(_ => 42, succeed(18))

  let () = doTest(Ok(42), fromResult(Ok(42)))
  let () = doTest(Error("failure"), fromResult(Error("failure")))

  let () = doTest(Ok(None), fail("for some reason") |> toOption)
  let () = doTest(Ok(Some(42)), succeed(42) |> toOption)
}
