open Tea.App

type msg =
  | Click
  | Set_value of int
  [@@bs.deriving {accessors}]


let update model = function
  | Click -> model + 1
  | Set_value n -> n


let view model =
  let open Tea.Html2 in
  let open Tea.Html2.Attributes in
  let open Tea.Html2.Events in
  let open Tea.Json in
  let clientX = Decoder.field "clientX" Decoder.int in
  div [] (List.map (fun e -> div [] [e]) [
    model |> string_of_int |> text;
    button [onClick Click] [text "onClick"];
    button [on "click" (Decoder.succeed Click)] [text "on \"click\""];
    a [href "https://www.google.com"] [text "a normal link"];
    a [
      href "https://www.google.com";
      onWithOptions "click" { defaultOptions with preventDefault = true } (Tea.Json.Decoder.succeed Click);
    ] [text "a link with prevent default"];
    button [on "click" (Decoder.map set_value clientX)] [text "on \"click\", use clientX value"];
    input' [type' "text"; on "input" (Decoder.map (fun v -> v |> int_of_string |> set_value) targetValue)] [];
  ])


let main =
  beginnerProgram {
    model = 0;
    update;
    view;
  }
