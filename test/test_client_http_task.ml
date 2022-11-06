open Tea
open Tea.Html

type msg =
  | GotResponse of (string, string) result
  | Req
  [@@bs.deriving accessors]

let update model = function
  | GotResponse (Ok t) -> t, Cmd.none
  | GotResponse (Error t) -> t, Cmd.none
  | Req -> model,
    Http.getString "https://jsonplaceholder.typicode.com/todos/1" |> Http.toTask |> Task.mapError Http.string_of_error
    |> Task.andThen (fun res -> Ex.LocalStorage.setItem "todo-1" res)
    |> Task.andThen (fun () -> Http.getString "https://jsonplaceholder.typicode.com/todos/2" |> Http.toTask |> Task.mapError Http.string_of_error)
    |> Task.andThen (fun res -> Ex.LocalStorage.setItem "todo-2" res)
    |> Task.andThen (fun () -> Task.succeed "both saved")
    |> Task.attempt gotResponse


let view model =
  div [] [
    button [onClick Req] [text "execute"];
    text model;
  ]

let som = function
  | GotResponse (Ok _) -> "GotResponse Ok"
  | GotResponse (Error _) -> "GotResponse Error"
  | Req -> "Req"

let main =
  Tea.Debug.standardProgram {
    init = (fun () -> "nothing", Cmd.none);
    subscriptions = (fun _ -> Sub.none);
    update;
    view;
  } som
