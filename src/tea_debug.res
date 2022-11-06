type debug_msg<'msg> =
  | ClientMsg('msg)
  | TogglePaused
  | SelectHistoryItem(int)
  | ToggleDetails
let client_msg = msg => ClientMsg(msg)

type state =
  | Running
  | Paused(int)

type debug_model<'model> = {
  history: list<(string, 'model)>,
  state: state,
  show_details: bool,
}

let debug = (
  string_of_msg: 'msg => string,
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
): (
  (('model, Tea_cmd.t<'msg>)) => (debug_model<'model>, Tea_cmd.t<debug_msg<'msg>>),
  (debug_model<'model>, debug_msg<'msg>) => (debug_model<'model>, Tea_cmd.t<debug_msg<'msg>>),
  debug_model<'model> => Vdom.t<debug_msg<'msg>>,
  debug_model<'model> => Tea_sub.t<debug_msg<'msg>>,
  debug_model<'model> => Tea_cmd.t<debug_msg<'msg>>,
) => {
  let init_debug = ((cmodel, cmd)) => (
    {
      history: list{("_init_", cmodel)},
      state: Running,
      show_details: false,
    },
    cmd |> Tea_cmd.map(client_msg),
  )

  let update' = (model, x) =>
    switch x {
    | ClientMsg(msg) =>
      if model.state == Running {
        let (_, cmodel) = List.hd(model.history)
        let (cmodel', cmd) = update(cmodel, msg)
        let dmodel' = {...model, history: list{(string_of_msg(msg), cmodel'), ...model.history}}
        (dmodel', cmd |> Tea_cmd.map(client_msg))
      } else {
        (model, Tea_cmd.none)
      }
    | TogglePaused =>
      switch model.state {
      | Paused(_) => ({...model, state: Running}, Tea_cmd.none)
      | Running => ({...model, state: Paused(0)}, Tea_cmd.none)
      }
    | SelectHistoryItem(i) => ({...model, state: Paused(i)}, Tea_cmd.none)
    | ToggleDetails => ({...model, show_details: !model.show_details}, Tea_cmd.none)
    }

  let view_styles = () => {
    open Tea_html2
    let rule = (selector, properties) =>
      properties
      |> List.map(((k, v)) => k ++ (":" ++ v))
      |> String.concat(";")
      |> (x => j`$(selector) {$(x)}`)
      |> text

    node(
      "style",
      list{},
      list{
        rule(
          "#debug.paused",
          list{
            ("position", "fixed"),
            ("top", "0"),
            ("left", "0"),
            ("width", "100%"),
            ("height", "100%"),
            ("pointer-events", "all"),
            ("background-color", "rgba(0,0,0,.1)"),
            ("box-shadow", "inset 0 0 10px #333"),
          },
        ),
        rule(
          "#debug nav",
          list{
            ("position", "fixed"),
            ("max-width", "50%"),
            ("bottom", "0"),
            ("right", "6px"),
            ("border-radius", "4px 4px 0 0"),
            ("background-color", "#444"),
            ("color", "#fff"),
            ("font-family", "monospace"),
            ("box-shadow", "0 0 10px #333"),
          },
        ),
        rule("#debug.paused nav", list{("height", "50%"), ("padding-bottom", "2em")}),
        rule(
          "#debug nav .toggle",
          list{
            ("padding", "6px"),
            ("padding-left", "9px"),
            ("cursor", "pointer"),
            ("min-width", "24ch"),
            ("text-align", "center"),
            ("border-left", "3px solid #333"),
            ("border-radius", "4px 4px 0 0"),
          },
        ),
        rule(
          "#debug nav .toggle:before",
          list{
            ("content", "' '"),
            ("position", "absolute"),
            ("left", "0"),
            ("top", "0"),
            ("width", ".5ch"),
            ("height", "1.8ch"),
            ("margin", "1.2ch"),
            ("border", "solid #fff"),
            ("border-width", "0 .5ch"),
          },
        ),
        rule(
          "#debug.paused nav .toggle:before",
          list{
            ("border-color", "transparent"),
            ("border-left-color", "#fff"),
            ("border-width", "1ch"),
            ("width", "0"),
            ("height", "0"),
          },
        ),
        rule(
          "#debug nav .history",
          list{
            ("margin", "0"),
            ("padding", "0"),
            ("height", "100%"),
            ("overflow-y", "auto"),
            ("list-style", "none"),
          },
        ),
        rule(
          "#debug nav .history li",
          list{("margin", "0"), ("padding", "0.2ch"), ("border-left", "3px solid #333")},
        ),
        rule("#debug nav .history li.selected", list{("background-color", "#333")}),
        rule(
          "#debug nav .history span.details",
          list{
            ("display", "inline-block"),
            ("cursor", "pointer"),
            ("width", "1ch"),
            ("margin", "0 1ch"),
            ("vertical-align", "super"),
          },
        ),
        rule("#debug nav .history li.selected span.details:after", list{("content", "'\\2026'")}),
        rule("#debug nav .history li.selected.show", list{("border-left", "3px solid white")}),
        rule(
          "#debug nav .history span.message",
          list{
            ("display", "inline-block"),
            ("cursor", "pointer"),
            ("white-space", "nowrap"),
            ("overflow", "hidden"),
            ("text-overflow", "ellipsis"),
            ("width", "calc(100% - 75px)"),
          },
        ),
        rule(
          "#debug nav .history span.index",
          list{
            ("display", "inline-block"),
            ("min-width", "3ch"),
            ("margin", "0 1ch"),
            ("color", "#aaa"),
            ("text-align", "right"),
            ("float", "right"),
          },
        ),
        rule(
          "#debug aside.details",
          list{
            ("position", "absolute"),
            ("width", "40ch"),
            ("top", "0"),
            ("bottom", "0"),
            ("right", "100%"),
            ("margin-right", "1.5ch"),
            ("overflow", "scroll"),
            ("background-color", "#fff"),
            ("color", "#000"),
            ("box-shadow", "0 0 10px #333"),
            ("border-radius", "4px 4px 0 0"),
            ("border", "2px solid #333"),
            ("padding", "1ch"),
            ("white-space", "pre"),
          },
        ),
      },
    )
  }

  let view_details = model => {
    open Tea_html2
    module A = Tea_html2.Attributes
    let format = %raw(`
      function (v) {
        var formatRecord = function (data, labels) {
          return data.reduce(
            function (acc, cur, index) {
              acc[labels[index]] = formatValue(cur)
              return acc
            }, {})
        }
        var listToArray = function (data) {
          var result = []
          var cur = data
          while (typeof cur !== "number") {
            result.push(formatValue(cur[0]))
            cur = cur[1]
          }
          return result
        }
        var formatVariant = function (data, recordVariant) {
          if (recordVariant === "::") {
            return listToArray(data)
          }
          else {
            return formatRecord(data, [recordVariant])
          }
        }
        var formatValue = function (x) {
          var recordLabels, recordVariant, recordModule, recordPolyVar
          if (x == null) {
            return null
          }
          else if ((recordLabels = x[Symbol.for('BsRecord')]) !== undefined) {
            return formatRecord(x, recordLabels)
          }
          else if ((recordModule = x[Symbol.for('BsLocalModule')]) !== undefined) {
            return formatRecord(x, recordModule)
          }
          else if ((recordVariant = x[Symbol.for('BsVariant')]) !== undefined) {
            return formatVariant(x, recordVariant)
          }
          else if ((recordPolyVar = x[Symbol.for('BsPolyVar')]) !== undefined) {
            return x[1]
          }
          else if (Array.isArray(x)) {
            // tuple
            return x.map(formatValue)
          }
          else {
            // scalar
            return x
          }
        }
        return JSON.stringify(formatValue(v), null, 2);
      }
    `)
    aside(list{A.class'("details")}, list{model |> format |> text})
  }

  let view_history = (model, selected_index) => {
    open Tea_html2
    module A = Tea_html2.Attributes
    module E = Tea_html2.Events
    let count = List.length(model.history)
    \"@@"(ul(list{A.class'("history")}), List.mapi((i, (msg, cmodel)) => {
        let selected = i == selected_index
        li(
          list{
            E.onClick(SelectHistoryItem(i)),
            A.classList(list{("selected", selected), ("show", selected && model.show_details)}),
          },
          list{
            span(
              list{
                A.classList(list{("details", true), ("show", true)}),
                ...if selected {
                  list{E.onClick(ToggleDetails), A.title("toggle details")}
                } else {
                  list{A.noProp, A.noProp}
                },
              },
              list{
                if selected && model.show_details {
                  view_details(cmodel)
                } else {
                  noNode
                },
              },
            ),
            span(list{A.class'("message")}, list{text(msg)}),
            span(list{A.class'("index")}, list{count - i |> string_of_int |> text}),
          },
        )
      }, model.history))
  }

  let view' = model => {
    open Tea_html2
    module A = Tea_html2.Attributes
    module E = Tea_html2.Events
    let (selected_index, selected_model, paused) = switch model.state {
    | Running => (0, List.hd(model.history) |> snd, false)
    | Paused(index) => (index, List.nth(model.history, index) |> snd, true)
    }

    let history_count = List.length(model.history)
    div(
      list{},
      list{
        view(selected_model) |> Tea_app.map(client_msg),
        div(
          list{A.id("debug"), A.classList(list{("paused", paused)})},
          list{
            view_styles(),
            nav(
              list{},
              list{
                div(
                  list{
                    A.class'("toggle"),
                    E.onClick(TogglePaused),
                    if paused {
                      A.title("click to resume")
                    } else {
                      A.title("click to pause")
                    },
                  },
                  list{j`Explore History ($(history_count))` |> text},
                ),
                if paused {
                  view_history(model, selected_index)
                } else {
                  noNode
                },
              },
            ),
          },
        ),
      },
    )
  }

  let subscriptions' = model =>
    model.history |> List.hd |> snd |> subscriptions |> Tea_sub.map(client_msg)

  let shutdown' = model => model.history |> List.hd |> snd |> shutdown |> Tea_cmd.map(client_msg)

  (init_debug, update', view', subscriptions', shutdown')
}

let debug_program: (
  'msg => string,
  Tea_app.program<'flags, 'model, 'msg>,
) => Tea_app.program<'flags, debug_model<'model>, debug_msg<'msg>> = (
  string_of_msg,
  {init, update, view, subscriptions, shutdown},
) => {
  let (init_debug, update', view', subscriptions', shutdown') = debug(
    string_of_msg,
    update,
    view,
    subscriptions,
    shutdown,
  )

  {
    init: flags => init(flags) |> init_debug,
    update: update',
    view: view',
    subscriptions: subscriptions',
    shutdown: shutdown',
  }
}

let debug_navigation_program: (
  'msg => string,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
) => Tea_navigation.navigationProgram<'flags, debug_model<'model>, debug_msg<'msg>> = (
  string_of_msg,
  {init, update, view, subscriptions, shutdown},
) => {
  let (init_debug, update', view', subscriptions', shutdown') = debug(
    string_of_msg,
    update,
    view,
    subscriptions,
    shutdown,
  )

  {
    init: (flags, location) => init(flags, location) |> init_debug,
    update: update',
    view: view',
    subscriptions: subscriptions',
    shutdown: shutdown',
  }
}

let beginnerProgram: (
  Tea_app.beginnerProgram<'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  unit,
) => Tea_app.programInterface<debug_msg<'msg>> = (
  {model, update, view},
  string_of_msg,
  pnode,
  flags,
) => {
  let debugged = debug_program(
    string_of_msg,
    {
      init: () => (model, Tea_cmd.none),
      update: (model, msg) => (update(model, msg), Tea_cmd.none),
      view: view,
      subscriptions: _model => Tea_sub.none,
      shutdown: _model => Tea_cmd.none,
    },
  )
  Tea_app.program(debugged, pnode, flags)
}

let standardProgram: (
  Tea_app.standardProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debug_msg<'msg>> = (
  {init, update, view, subscriptions},
  string_of_msg,
  pnode,
  flags,
) => {
  let debugged = debug_program(
    string_of_msg,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: _model => Tea_cmd.none,
    },
  )
  Tea_app.program(debugged, pnode, flags)
}

let program: (
  Tea_app.program<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debug_msg<'msg>> = (
  {init, update, view, subscriptions, shutdown},
  string_of_msg,
  pnode,
  flags,
) => {
  let debugged = debug_program(
    string_of_msg,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: shutdown,
    },
  )
  Tea_app.program(debugged, pnode, flags)
}

let navigationProgram: (
  Tea_navigation.Location.t => 'msg,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debug_msg<'msg>> = (
  location_to_msg,
  {init, update, view, subscriptions, shutdown},
  string_of_msg,
  pnode,
  flags,
) => {
  let location = location => location |> location_to_msg |> client_msg

  let debugged = debug_navigation_program(
    string_of_msg,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: shutdown,
    },
  )
  Tea_navigation.navigationProgram(location, debugged, pnode, flags)
}
