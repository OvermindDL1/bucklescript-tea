type program<'flags, 'model, 'msg> = {
  init: 'flags => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
}

type standardProgram<'flags, 'model, 'msg> = {
  init: 'flags => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
}

type beginnerProgram<'model, 'msg> = {
  model: 'model,
  update: ('model, 'msg) => 'model,
  view: 'model => Vdom.t<'msg>,
}

type pumpInterface<'model, 'msg> = {
  startup: unit => unit,
  render_string: 'model => string,
  handleMsg: ('model, 'msg) => 'model,
  shutdown: Tea_cmd.t<'msg> => unit,
}

type programInterface<'msg> = {
  "getHtmlString": unit => string,
  "pushMsg": 'msg => unit,
  "shutdown": unit => unit,
}

@obj
external makeProgramInterface: (
  ~pushMsg: 'msg => unit,
  ~shutdown: unit => unit,
  ~getHtmlString: unit => string,
) => programInterface<'msg> = ""

let programStateWrapper: (
  'msg,
  ref<Vdom.applicationCallbacks<'msg>> => pumpInterface<'msg, 'msg>,
  'msg => Tea_cmd.t<'msg>,
) => programInterface<'msg>

let programLoop: (
  ('msg, 'b) => ('msg, Tea_cmd.t<'b>),
  'msg => Vdom.t<'b>,
  'msg => Tea_sub.t<'b>,
  'msg,
  Tea_cmd.t<'b>,
  option<Dom.node>,
  ref<Tea_cmd.applicationCallbacks<'b>>,
) => pumpInterface<'msg, 'b>

let program: (
  program<'flags, 'model, 'msg>,
  Js.null_undefined<Dom.node>,
  'flags,
) => programInterface<'msg>

let standardProgram: (
  standardProgram<'flags, 'model, 'msg>,
  Js.null_undefined<Dom.node>,
  'flags,
) => programInterface<'msg>

let beginnerProgram: (
  beginnerProgram<'model, 'msg>,
  Js.null_undefined<Dom.node>,
  unit,
) => programInterface<'msg>

let map: ('msg => 'b, Vdom.t<'msg>) => Vdom.t<'b>
