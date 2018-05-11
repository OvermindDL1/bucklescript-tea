open Tea.App;

open Tea.Html;

type msg =
  | Increment
  | Decrement
  | Reset
  | Set(int);

let update = model =>
  fun
  | Increment => model + 1
  | Decrement => model - 1
  | Reset => 0
  | Set(v) => v;

let view_button = (title, msg) => button([onClick(msg)], [text(title)]);

let view = model =>
  div(
    [],
    [
      span([style("text-weight", "bold")], [text(string_of_int(model))]),
      br([]),
      view_button(
        "Increment",
        if (model >= 3) {
          Decrement;
        } else {
          Increment;
        },
      ),
      br([]),
      view_button("Decrement", Decrement),
      br([]),
      view_button("Set to 42", Set(42)),
      br([]),
      if (model != 0) {
        view_button("Reset", Reset);
      } else {
        noNode;
      },
    ],
  );

let main = beginnerProgram({model: 4, update, view});
