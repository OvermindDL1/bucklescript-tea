open Tea.App;

open Tea.Html;

[@bs.deriving {accessors: accessors}]
type msg =
  | Trigger;

type model = (option(string), option(string));

let update' = model =>
  fun
  | Trigger => {
      let (left, _) = model;
      (left, Some("right"));
    };

let render_model =
  fun
  | (Some(_), Some(_)) => input'([value("This should be on screen")], [])
  | _ => span([], [text("nothing")]);

let view' = model =>
  div(
    [],
    [
      button([onClick(Trigger)], [text("trigger rerender")]),
      render_model(model),
    ],
  );

let main =
  beginnerProgram({
    model: (Some("left"), None),
    update: update',
    view: view',
  });
