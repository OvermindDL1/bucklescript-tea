type never

type ('succeed,'fail) t =
  | Task: ((('succeed,'fail) Tea_result.t -> unit) -> unit) ->
  ('succeed,'fail) t

let nothing () = ()

let performOpt (toOptionalMessage : 'value -> 'msg option)
  (((Task (task))[@explicit_arity ]) : ('value,never) t) =
  (Tea_cmd.call
     (fun callbacks  ->
        let open Tea_result in
          let open Vdom in
            let cb =
              function
              | ((Error (_e))[@explicit_arity ]) ->
                  failwith
                    (("ERROR:  Task perfom returned error of never! Should not happen!")
                    [@reason.raw_literal
                      "ERROR:  Task perfom returned error of never! Should not happen!"])
              | ((Ok (v))[@explicit_arity ]) ->
                  (match toOptionalMessage v with
                   | None  -> ()
                   | ((Some (result))[@explicit_arity ]) ->
                       (!callbacks).enqueue result) in
            task cb) : 'msg Tea_cmd.t)

let perform (toMessage : 'value -> 'msg) (task : ('value,never) t) =
  (performOpt (fun v  -> ((Some ((toMessage v)))[@explicit_arity ])) task :
  'msg Tea_cmd.t)

let attemptOpt
  (resultToOptionalMessage : ('succeed,'fail) Tea_result.t -> 'msg option)
  (((Task (task))[@explicit_arity ]) : ('succeed,'fail) t) =
  (Tea_cmd.call
     (fun callbacks  ->
        let open Vdom in
          let cb value =
            match resultToOptionalMessage value with
            | None  -> ()
            | ((Some (result))[@explicit_arity ]) ->
                (!callbacks).enqueue result in
          task cb) : 'msg Tea_cmd.t)

let attempt (resultToMessage : ('succeed,'fail) Tea_result.t -> 'msg)
  (task : ('succeed,'fail) t) =
  (attemptOpt (fun v  -> ((Some ((resultToMessage v)))[@explicit_arity ]))
     task : 'msg Tea_cmd.t)

let ignore task = attemptOpt (fun _ -> None) task

let succeed (value : 'v) =
  (((Task ((fun cb  -> cb ((Tea_result.Ok (value))[@explicit_arity ]))))
  [@explicit_arity ]) : ('v,'e) t)

let fail (value : 'v) =
  (((Task ((fun cb  -> cb ((Tea_result.Error (value))[@explicit_arity ]))))
  [@explicit_arity ]) : ('e,'v) t)

let nativeBinding (func : (('succeed,'fail) Tea_result.t -> unit) -> unit) =
  (((Task (func))[@explicit_arity ]) : ('succeed,'fail) t)

let andThen fn ((Task (task))[@explicit_arity ]) =
  let open Tea_result in
    ((Task
        ((fun cb  ->
            task
              (function
               | ((Error (_e))[@explicit_arity ]) as err -> cb err
               | ((Ok (v))[@explicit_arity ]) ->
                   let ((Task (nextTask))[@explicit_arity ]) = fn v in
                   nextTask cb))))[@explicit_arity ])

let onError fn ((Task (task))[@explicit_arity ]) =
  let open Tea_result in
    ((Task
        ((fun cb  ->
            task
              (function
               | ((Ok (_v))[@explicit_arity ]) as ok -> cb ok
               | ((Error (e))[@explicit_arity ]) ->
                   let ((Task (newTask))[@explicit_arity ]) = fn e in
                   newTask cb))))[@explicit_arity ])

let fromResult : ('success,'failure) Tea_result.t -> ('success,'failure) t = function
  | Tea_result.Ok s -> succeed s
  | Tea_result.Error err -> fail err

let mapError func task = task |> (onError (fun e  -> fail (func e)))

let toOption task =
  task
  |> andThen (fun v -> succeed (Some v))
  |> onError (fun _ -> succeed None)

let map func task1 = task1 |> (andThen (fun v1  -> succeed (func v1)))

let map2 func task1 task2 =
  task1 |>
    (andThen
       (fun v1  -> task2 |> (andThen (fun v2  -> succeed (func v1 v2)))))

let map3 func task1 task2 task3 =
  task1 |>
    (andThen
       (fun v1  ->
          task2 |>
            (andThen
               (fun v2  ->
                  task3 |> (andThen (fun v3  -> succeed (func v1 v2 v3)))))))

let map4 func task1 task2 task3 task4 =
  task1 |>
    (andThen
       (fun v1  ->
          task2 |>
            (andThen
               (fun v2  ->
                  task3 |>
                    (andThen
                       (fun v3  ->
                          task4 |>
                            (andThen (fun v4  -> succeed (func v1 v2 v3 v4)))))))))

let map5 func task1 task2 task3 task4 task5 =
  task1 |>
    (andThen
       (fun v1  ->
          task2 |>
            (andThen
               (fun v2  ->
                  task3 |>
                    (andThen
                       (fun v3  ->
                          task4 |>
                            (andThen
                               (fun v4  ->
                                  task5 |>
                                    (andThen
                                       (fun v5  ->
                                          succeed (func v1 v2 v3 v4 v5)))))))))))

let map6 func task1 task2 task3 task4 task5 task6 =
  task1 |>
    (andThen
       (fun v1  ->
          task2 |>
            (andThen
               (fun v2  ->
                  task3 |>
                    (andThen
                       (fun v3  ->
                          task4 |>
                            (andThen
                               (fun v4  ->
                                  task5 |>
                                    (andThen
                                       (fun v5  ->
                                          task6 |>
                                            (andThen
                                               (fun v6  ->
                                                  succeed
                                                    (func v1 v2 v3 v4 v5 v6)))))))))))))

let rec sequence =
  function
  | [] -> succeed []
  | task::remainingTasks ->
      map2 (fun l  -> fun r  -> l :: r) task (sequence remainingTasks)

let testing_deop = ref true

let testing () =
  let open Tea_result in
    let doTest expected ((Task (task))[@explicit_arity ]) =
      let testAssert v =
        if v = expected
        then
          Js.log ((("Passed:")[@reason.raw_literal "Passed:"]), expected, v)
        else
          Js.log ((("FAILED:")[@reason.raw_literal "FAILED:"]), expected, v) in
      task testAssert in
    let s = succeed 42 in
    let () = doTest ((Ok (42))[@explicit_arity ]) s in
    let f = fail 86 in
    let () = doTest ((Error (86))[@explicit_arity ]) f in
    let r () = if !testing_deop then succeed 42 else fail 3.14 in
    let a1 = (succeed 2) |> (andThen (fun n  -> succeed (n + 2))) in
    let () = doTest ((Ok (4))[@explicit_arity ]) a1 in
    let a2 = (succeed 2) |> (andThen (fun n  -> succeed (string_of_int n))) in
    let () =
      doTest ((Ok ((("2")[@reason.raw_literal "2"])))[@explicit_arity ]) a2 in
    let m1 = map sqrt (succeed 9.) in
    let () = doTest ((Ok (3.))[@explicit_arity ]) m1 in
    let m2 = map2 (+) (succeed 9) (succeed 3) in
    let () = doTest ((Ok (12))[@explicit_arity ]) m2 in
    let m3 = map string_of_int (succeed 9) in
    let () =
      doTest ((Ok ((("9")[@reason.raw_literal "9"])))[@explicit_arity ]) m3 in
    let s0 = sequence [succeed 1; succeed 2] in
    let () = doTest ((Ok ([1; 2]))[@explicit_arity ]) s0 in
    let s1 = sequence [succeed 1; fail 2.7; r ()] in
    let () = doTest ((Error (2.7))[@explicit_arity ]) s1 in
    let e0 =
      (fail (("file not found")[@reason.raw_literal "file not found"])) |>
        (onError (fun _msg  -> succeed 42)) in
    let () = doTest ((Ok (42))[@explicit_arity ]) e0 in
    let e1 =
      (fail (("file not found")[@reason.raw_literal "file not found"])) |>
        (onError (fun _msg  -> fail 42)) in
    let () = doTest ((Error (42))[@explicit_arity ]) e1 in
    let n0 =
      sequence
        [mapError string_of_int (fail 42);
        mapError Js.Float.toString (fail 3.14)] in
    let () =
      doTest ((Error ((("42")[@reason.raw_literal "42"])))[@explicit_arity ])
        n0 in
    let n1 =
      sequence
        [mapError string_of_int (succeed 1);
        mapError Js.Float.toString (fail 3.14)] in
    let () =
      doTest
        ((Error ((("3.14")[@reason.raw_literal "3.14"])))[@explicit_arity ])
        n1 in
    let n2 =
      sequence
        [mapError string_of_int (succeed 1);
        mapError Js.Float.toString (succeed 2)] in
    let () = doTest ((Ok ([1; 2]))[@explicit_arity ]) n2 in

    let _c0 = perform (fun _  -> 42) (succeed 18) in
    
    let () = doTest (Ok 42) (fromResult (Ok 42)) in
    let () = doTest (Error "failure") (fromResult (Error "failure")) in
    
    let () = doTest (Ok None) (fail "for some reason" |> toOption) in
    let () = doTest (Ok (Some 42)) (succeed 42 |> toOption) in

    ()
