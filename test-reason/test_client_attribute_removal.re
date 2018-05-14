open Tea.App;

open Tea.Html;

type model = {
  selected: option(string),
  languages: list(string),
};

[@bs.deriving {accessors: accessors}]
type message =
  | Select(string)
  | Delete;

let render_selected =
  fun
  | Some(selected) =>
    div(
      [],
      [
        text("you selected " ++ selected),
        div([onClick(Delete)], [text("delete selection")]),
      ],
    )
  | None => div([], [text("Nothing selected")]);

/* let lang l is_selected =
 *   let baseProps = [onClick (Select l); style "color" "blue"] in
 *   let props = if is_selected == true then (style "border" "1px solid black")::baseProps else baseProps
 *   in
 *   li props [text l] */
let lang = (l, is_selected) =>
  li(
    [
      onClick(Select(l)),
      style("color", "blue"),
      if (is_selected) {
        style("border", "1px solid black");
      } else {
        noProp;
      },
      if (is_selected) {
        Vdom.attribute("", "lang", l);
      } else {
        noProp;
      },
    ],
    [text(l)],
  );

let render_languages = (selected, languages) => {
  let is_selected = (selected, language) =>
    switch (selected) {
    | Some(l) => language === l
    | None => false
    };
  let rendered =
    List.map(l => lang(l, is_selected(selected, l)), languages);
  ul([], rendered);
};

let update = state =>
  fun
  | Select(lang) => {...state, selected: Some(lang)}
  | Delete => {...state, selected: None};

let view = state =>
  div(
    [],
    [
      render_selected(state.selected),
      render_languages(state.selected, state.languages),
    ],
  );

let main = {
  let initialState = {
    selected: Some("Erlang"),
    languages: ["Erlang", "Ocaml", "Clojure"],
  };
  beginnerProgram({model: initialState, update, view});
};
