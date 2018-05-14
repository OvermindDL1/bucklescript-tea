open Tea.App;

[@bs.deriving {accessors: accessors}]
type msg =
  | Click
  | Set_value(int);

let update = model =>
  fun
  | Click => model + 1
  | Set_value(n) => n;

let view = model => {
  open Tea.Html2;
  open Tea.Html2.Attributes;
  open Tea.Html2.Events;
  open Tea.Json;
  let clientX = Decoder.field("clientX", Decoder.int);
  div(
    [],
    List.map(
      e => div([], [e]),
      [
        model |> string_of_int |> text,
        button([onClick(Click)], [text("onClick")]),
        button(
          [on("click", Decoder.succeed(Click))],
          [text("on \"click\"")],
        ),
        a([href("https://www.google.com")], [text("a normal link")]),
        a(
          [
            href("https://www.google.com"),
            onWithOptions(
              "click",
              {...defaultOptions, preventDefault: true},
              Tea.Json.Decoder.succeed(Click),
            ),
          ],
          [text("a link with prevent default")],
        ),
        button(
          [on("click", Decoder.map(set_value, clientX))],
          [text("on \"click\", use clientX value")],
        ),
        input'(
          [
            type'("text"),
            on(
              "input",
              Decoder.map(v => v |> int_of_string |> set_value, targetValue),
            ),
          ],
          [],
        ),
      ],
    ),
  );
};

let main = beginnerProgram({model: 0, update, view});
