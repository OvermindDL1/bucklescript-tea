open Tea.Html

module BeginnerApp = struct
  type msg =
    | Trigger
  [@@bs.deriving {accessors}]

  type model = (string option * string option)

  let update model = function
    | Trigger ->
      let (left, _) = model in
      (left, Some "right")

  let render_model = function
    | (Some _, Some _) ->
      input' [value "This should be on screen"] []
    | _ ->
      span [] [text "nothing"]

  let view model =
    div []
      [ button [onClick Trigger] [text "trigger rerender"]
      ; render_model model
      ]

  let init = (Some "left", None);
end

module P = Tea.App.MakeBeginner(BeginnerApp)
let main = P.program
