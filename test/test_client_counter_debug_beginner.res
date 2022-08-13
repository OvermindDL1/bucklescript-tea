open Tea.App
open Tea.Html

type msg =
  | Increment
  | Decrement
  | Reset
  | Set(int)

let string_of_msg = x =>
  switch x {
  | Increment => "Increment"
  | Decrement => "Decrement"
  | Reset => "Reset"
  | Set(_) => "Set"
  }

let update = (model, x) =>
  switch x {
  | Increment => model + 1
  | Decrement => model - 1
  | Reset => 0
  | Set(v) => v
  }

let view_button = (title, msg) => button(list{onClick(msg)}, list{text(title)})

let view = model =>
  div(
    list{},
    list{
      span(list{style("text-weight", "bold")}, list{text(string_of_int(model))}),
      br(list{}),
      view_button(
        "Increment",
        if model >= 3 {
          Decrement
        } else {
          Increment
        },
      ),
      br(list{}),
      view_button("Decrement", Decrement),
      br(list{}),
      view_button("Set to 42", Set(42)),
      br(list{}),
      if model != 0 {
        view_button("Reset", Reset)
      } else {
        noNode
      },
    },
  )

let main = Tea.Debug.beginnerProgram(
  {
    model: 4,
    update: update,
    view: view,
  },
  string_of_msg,
)
