open Tea;
open Tea.Html;
type result('ok, 'err) = Tea_result.t('ok, 'err);

[@bs.deriving accessors]
type msg =
  | GotResponse(result(string, string))
  | Req;

let update = model =>
  fun
  | GotResponse(Ok(t)) => (t, Cmd.none)
  | GotResponse(Error(t)) => (t, Cmd.none)
  | Req => (
      model,
      Http.getString("https://jsonplaceholder.typicode.com/todos/1")
      |> Http.toTask
      |> Task.mapError(Http.string_of_error)
      |> Task.andThen(res => Ex.LocalStorage.setItem("todo-1", res))
      |> Task.andThen(() =>
           Http.getString("https://jsonplaceholder.typicode.com/todos/2")
           |> Http.toTask
           |> Task.mapError(Http.string_of_error)
         )
      |> Task.andThen(res => Ex.LocalStorage.setItem("todo-2", res))
      |> Task.andThen(() => Task.succeed("both saved"))
      |> Task.attempt(gotResponse),
    );

let view = model =>
  div([], [button([onClick(Req)], [text("execute")]), text(model)]);

let som =
  fun
  | GotResponse(Ok(_)) => "GotResponse Ok"
  | GotResponse(Error(_)) => "GotResponse Error"
  | Req => "Req";

let main =
  Tea.Debug.standardProgram(
    {
      init: () => ("nothing", Cmd.none),
      subscriptions: _ => Sub.none,
      update,
      view,
    },
    som,
  );
