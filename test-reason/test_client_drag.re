open Tea;

open Tea.App;

open Tea.Html;

open Tea.Mouse;

[@bs.deriving {accessors: accessors}]
type msg =
  | DragStart(position)
  | DragAt(position)
  | DragEnd(position);

type drag = {
  start: position,
  current: position,
};

type model = {
  position,
  drag: option(drag),
};

let init = () => ({
                    position: {
                      x: 200,
                      y: 200,
                    },
                    drag: None,
                  }, Cmd.none);

let getPosition = ({position, drag}) =>
  switch (drag) {
  | None => position
  | Some({start, current}) => {
      x: position.x + current.x - start.x,
      y: position.y + current.y - start.y,
    }
  };

let updateHelp = ({position} as model) =>
  fun
  | DragStart(xy) => {position, drag: Some({start: xy, current: xy})}
  | DragAt(xy) => {
      position,
      drag:
        switch (model.drag) {
        | None => None
        | Some(drag) => Some({...drag, current: xy})
        },
    }
  | DragEnd(_) => {position: getPosition(model), drag: None};

let update = (model, msg) => (updateHelp(model, msg), Cmd.none);

let subscriptions = model =>
  switch (model.drag) {
  | None => Sub.none
  | Some(_) => Sub.batch([Mouse.moves(dragAt), Mouse.ups(dragEnd)])
  };

let px = number => string_of_int(number) ++ "px";

let onMouseDown =
  onCB("mousedown", "", ev =>
    Json.Decoder.decodeEvent(Json.Decoder.map(dragStart, Mouse.position), ev)
    |> Result.result_to_option
  );

let view = model => {
  let realPosition = getPosition(model);
  div(
    [
      onMouseDown,
      styles([
        ("background-color", "#3C8D2F"),
        ("cursor", "move"),
        ("width", "100px"),
        ("height", "100px"),
        ("border-radius", "4px"),
        ("position", "absolute"),
        ("left", px(realPosition.x)),
        ("top", px(realPosition.y)),
        ("color", "white"),
        ("display", "flex"),
        ("align-items", "center"),
        ("justify-content", "center"),
      ]),
    ],
    [text("Drag Me!")],
  );
};

let main = standardProgram({init, update, view, subscriptions});
