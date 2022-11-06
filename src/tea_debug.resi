type debug_msg<'msg> =
  | ClientMsg('msg)
  | TogglePaused
  | SelectHistoryItem(int)
  | ToggleDetails

let client_msg: 'a => debug_msg<'a>

type state = Running | Paused(int)

type debug_model<'model> = {
  history: list<(string, 'model)>,
  state: state,
  show_details: bool,
}

let debug: (
  'msg => string,
  ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  'model => Vdom.t<'msg>,
  'model => Tea_sub.t<'msg>,
  'model => Tea_cmd.t<'msg>,
) => (
  (('model, Tea_cmd.t<'msg>)) => (debug_model<'model>, Tea_cmd.t<debug_msg<'msg>>),
  (debug_model<'model>, debug_msg<'msg>) => (debug_model<'model>, Tea_cmd.t<debug_msg<'msg>>),
  debug_model<'model> => Vdom.t<debug_msg<'msg>>,
  debug_model<'model> => Tea_sub.t<debug_msg<'msg>>,
  debug_model<'model> => Tea_cmd.t<debug_msg<'msg>>,
)

let debug_program: (
  'msg => string,
  Tea_app.program<'flags, 'model, 'msg>,
) => Tea_app.program<'flags, debug_model<'model>, debug_msg<'msg>>

let debug_navigation_program: (
  'msg => string,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
) => Tea_navigation.navigationProgram<'flags, debug_model<'model>, debug_msg<'msg>>

let beginnerProgram: (
  Tea_app.beginnerProgram<'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  unit,
) => Tea_app.programInterface<debug_msg<'msg>>

let standardProgram: (
  Tea_app.standardProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debug_msg<'msg>>

let program: (
  Tea_app.program<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debug_msg<'msg>>

let navigationProgram: (
  Tea_navigation.Location.t => 'msg,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debug_msg<'msg>>
