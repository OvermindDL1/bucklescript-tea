open Tea.App
open Tea.Html

@deriving({accessors: accessors}) type msg = Trigger

type model = (option<string>, option<string>)

let update' = (model, x) =>
  switch x {
  | Trigger =>
    let (left, _) = model
    (left, Some("right"))
  }

let render_model = x =>
  switch x {
  | (Some(_), Some(_)) => input'(list{value("This should be on screen")}, list{})
  | _ => span(list{}, list{text("nothing")})
  }

let view' = model =>
  div(
    list{},
    list{button(list{onClick(Trigger)}, list{text("trigger rerender")}), render_model(model)},
  )

let main = beginnerProgram({
  model: (Some("left"), None),
  update: update',
  view: view',
})
