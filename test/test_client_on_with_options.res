open Tea.App

@deriving({accessors: accessors})
type msg =
  | Click
  | Set_value(int)

let update = (model, x) =>
  switch x {
  | Click => model + 1
  | Set_value(n) => n
  }

let view = model => {
  open Tea.Html2
  open Tea.Html2.Attributes
  open Tea.Html2.Events
  open Tea.Json
  let clientX = Decoder.field("clientX", Decoder.int)
  div(
    list{},
    List.map(
      e => div(list{}, list{e}),
      list{
        model |> string_of_int |> text,
        button(list{onClick(Click)}, list{text("onClick")}),
        button(list{on(~key="", "click", Decoder.succeed(Click))}, list{text("on \"click\"")}),
        a(list{href("https://www.google.com")}, list{text("a normal link")}),
        a(
          list{
            href("https://www.google.com"),
            onWithOptions(
              ~key="",
              "click",
              {...defaultOptions, preventDefault: true},
              Tea.Json.Decoder.succeed(Click),
            ),
          },
          list{text("a link with prevent default")},
        ),
        button(
          list{on(~key="", "click", Decoder.map(set_value, clientX))},
          list{text("on \"click\", use clientX value")},
        ),
        input'(
          list{
            type'("text"),
            on(~key="", "input", Decoder.map(v => v |> int_of_string |> set_value, targetValue)),
          },
          list{},
        ),
      },
    ),
  )
}

let main = beginnerProgram({
  model: 0,
  update: update,
  view: view,
})
