type 'msg debug_msg =
  | ClientMsg of 'msg
  | TogglePaused
  | SelectHistoryItem of int
  | ToggleDetails
let client_msg msg = ClientMsg msg

type state =
  | Running
  | Paused of int

type 'model debug_model = {
  history : (string * 'model) list;
  state : state;
  show_details : bool;
}

let debug
  (string_of_msg : 'msg -> string)
  (update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t)
  (view : 'model -> 'msg Vdom.t)
  (subscriptions : 'model -> 'msg Tea_sub.t)
  (shutdown : 'model -> 'msg Tea_cmd.t) :
  ( ('model * 'msg Tea_cmd.t -> 'model debug_model * 'msg debug_msg Tea_cmd.t) *
    ('model debug_model -> 'msg debug_msg -> 'model debug_model * 'msg debug_msg Tea_cmd.t) *
    ('model debug_model -> 'msg debug_msg Vdom.t)  *
    ('model debug_model -> 'msg debug_msg Tea_sub.t) *
    ('model debug_model -> 'msg debug_msg Tea_cmd.t)
  )
  =
  let init_debug (cmodel, cmd) =
    {
      history = ["_init_", cmodel];
      state = Running;
      show_details = false;
    }, cmd |> Tea_cmd.map client_msg
  in

  let update' model = function
    | ClientMsg msg ->
      if model.state = Running then
        let _,cmodel = List.hd model.history in
        let cmodel',cmd = update cmodel msg in
        let dmodel' = { model with history = (string_of_msg msg, cmodel') :: model.history } in
        dmodel', cmd |> Tea_cmd.map client_msg
      else
        model, Tea_cmd.none
    | TogglePaused ->
      begin match model.state with
      | Paused _ -> { model with state = Running }, Tea_cmd.none
      | Running -> { model with state = Paused 0 }, Tea_cmd.none
      end
    | SelectHistoryItem i ->
      { model with state = Paused i }, Tea_cmd.none
    | ToggleDetails ->
      { model with show_details = not model.show_details }, Tea_cmd.none
  in

  let view_styles () =
    let open Tea_html2 in
    let rule selector properties =
      properties |> List.map (fun (k,v) -> k ^ ":" ^ v) |> String.concat ";" |> Printf.sprintf "%s {%s}" selector |> text
    in
    node "style" [] [
      rule "#debug.paused" [
        "position", "fixed"; "top", "0"; "left", "0"; "width", "100%"; "height", "100%"; "pointer-events", "all";
        "background-color", "rgba(0,0,0,.1)";
        "box-shadow", "inset 0 0 10px #333";
      ];
      rule "#debug nav" [
        "position", "fixed"; "max-width", "50%"; "bottom", "0"; "right", "6px";
        "border-radius", "4px 4px 0 0"; "background-color", "#444"; "color", "#fff"; "font-family", "monospace";
        "box-shadow", "0 0 10px #333";
      ];
      rule "#debug.paused nav" ["height", "50%"; "padding-bottom", "2em"];
      rule "#debug nav .toggle" [
        "padding", "6px";
        "padding-left", "9px";
        "cursor", "pointer";
        "min-width", "24ch";
        "text-align", "center";
        "border-left", "3px solid #333";
        "border-radius", "4px 4px 0 0"
      ];
      rule "#debug nav .toggle:before" [
        "content", "' '";
        "position", "absolute"; "left", "0"; "top", "0";
        "width", ".5ch"; "height", "1.8ch";
        "margin", "1.2ch";
        "border", "solid #fff";
        "border-width", "0 .5ch";
      ];
      rule "#debug.paused nav .toggle:before" [
        "border-color", "transparent"; "border-left-color", "#fff";
        "border-width", "1ch";
        "width", "0"; "height", "0";
      ];
      rule "#debug nav .history" [
        "margin", "0"; "padding", "0";
        "height", "100%";
        "overflow-y", "auto";
        "list-style", "none";
      ];
      rule "#debug nav .history li" ["margin", "0"; "padding", "0.2ch"; "border-left", "3px solid #333"];
      rule "#debug nav .history li.selected" ["background-color", "#333"];
      rule "#debug nav .history span.details" [
        "display", "inline-block"; "cursor", "pointer";
        "width", "1ch";
        "margin", "0 1ch";
        "vertical-align", "super";
      ];
      rule "#debug nav .history li.selected span.details:after" ["content", "'\\2026'"];
      rule "#debug nav .history li.selected.show" ["border-left", "3px solid white"];
      rule "#debug nav .history span.message" [
        "display", "inline-block"; "cursor", "pointer";
        "white-space", "nowrap"; "overflow", "hidden"; "text-overflow", "ellipsis";
        "width", "calc(100% - 75px)"
      ];
      rule "#debug nav .history span.index" [
        "display", "inline-block";
        "min-width", "3ch"; "margin", "0 1ch";
        "color", "#aaa"; "text-align", "right";
        "float", "right";
      ];
      rule "#debug aside.details" [
        "position", "absolute";
        "width", "40ch"; "top", "0"; "bottom", "0"; "right", "100%";
        "margin-right", "1.5ch";
        "overflow", "scroll";
        "background-color", "#fff"; "color", "#000";
        "box-shadow", "0 0 10px #333";
        "border-radius", "4px 4px 0 0";
        "border", "2px solid #333";
        "padding", "1ch";
        "white-space", "pre";
      ];
    ]
  in

  let view_details model =
    let open Tea_html2 in
    let module A = Tea_html2.Attributes in
    let format = [%raw {|
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
    |}] in
    aside [A.class' "details"] [model |> format |> text]
  in

  let view_history model selected_index =
    let open Tea_html2 in
    let module A = Tea_html2.Attributes in
    let module E = Tea_html2.Events in
    let count = List.length model.history in
    ul [A.class' "history"] @@
      List.mapi (fun i (msg, cmodel) ->
          let selected = i = selected_index in
          li [
            E.onClick (SelectHistoryItem i);
            A.classList ["selected", selected; "show", selected && model.show_details]
          ] [
            span (
              A.classList ["details", true; "show", true] ::
              if selected
              then [E.onClick ToggleDetails; A.title "toggle details"]
              else [A.noProp; A.noProp]
            ) [
              if selected && model.show_details
              then view_details cmodel
              else noNode
            ];
            span [A.class' "message"] [text msg];
            span [A.class' "index"] [(count - i) |> string_of_int |> text];
          ]
      ) model.history
  in

  let view' model =
    let open Tea_html2 in
    let module A = Tea_html2.Attributes in
    let module E = Tea_html2.Events in
    let selected_index, selected_model, paused = match model.state with
      | Running -> 0, (List.hd model.history |> snd), false
      | Paused index -> index, (List.nth model.history index |> snd), true
    in
    let history_count = List.length model.history in
    div [] [
      view selected_model |> Tea_app.map client_msg;
      div [A.id "debug"; A.classList ["paused", paused]] [
        view_styles ();
        nav [] [
          div [
            A.class' "toggle"; E.onClick TogglePaused;
            if paused then A.title "click to resume"
            else A.title "click to pause"
          ] [
            Printf.sprintf "Explore History (%d)" history_count |> text
          ];
          if paused then
            view_history model selected_index
          else
            noNode;
        ];
      ];
    ]
  in

  let subscriptions' model =
    model.history |> List.hd |> snd |> subscriptions |> Tea_sub.map client_msg
  in

  let shutdown' model =
    model.history |> List.hd |> snd |> shutdown |> Tea_cmd.map client_msg
  in

  ( init_debug,
    update',
    view',
    subscriptions',
    shutdown'
  )


let debug_program :
  ('msg -> string) ->
  ('flags, 'model, 'msg) Tea_app.program ->
  ('flags, 'model debug_model, 'msg debug_msg) Tea_app.program
  =
  fun string_of_msg { init; update; view; subscriptions; shutdown } ->
    let (init_debug, update', view', subscriptions', shutdown') = debug
      string_of_msg
      update
      view
      subscriptions
      shutdown
    in

    {
      init = (fun flags -> init flags |> init_debug);
      update = update';
      view = view';
      subscriptions = subscriptions';
      shutdown = shutdown';
    }


let debug_navigation_progam :
  ('msg -> string) ->
  ('flags, 'model, 'msg) Tea_navigation.navigationProgram ->
  ('flags, 'model debug_model, 'msg debug_msg) Tea_navigation.navigationProgram
  =
  fun string_of_msg { init; update; view; subscriptions; shutdown } ->
    let (init_debug, update', view', subscriptions', shutdown') = debug
      string_of_msg
      update
      view
      subscriptions
      shutdown
    in

    {
      init = (fun flags location -> init flags location |> init_debug);
      update = update';
      view = view';
      subscriptions = subscriptions';
      shutdown = shutdown';
    }



let beginnerProgram :
  ('model, 'msg) Tea_app.beginnerProgram ->
  ('msg -> string) ->
  Web.Node.t Js.null_undefined ->
  unit ->
  'msg debug_msg Tea_app.programInterface
  = fun { model; update; view; } string_of_msg pnode flags ->
    let debugged = debug_program
      string_of_msg
      {
        init = (fun () -> model, Tea_cmd.none);
        update = (fun model msg -> update model msg, Tea_cmd.none);
        view;
        subscriptions = (fun _model -> Tea_sub.none);
        shutdown = (fun _model -> Tea_cmd.none);
      }
    in Tea_app.program debugged pnode flags


let standardProgram :
  ('flags, 'model, 'msg) Tea_app.standardProgram ->
  ('msg -> string) ->
  Web.Node.t Js.null_undefined ->
  'flags ->
  'msg debug_msg Tea_app.programInterface
  = fun { init; update; view; subscriptions } string_of_msg pnode flags ->
    let debugged = debug_program
      string_of_msg
      {
        init;
        update;
        view;
        subscriptions;
        shutdown = (fun _model -> Tea_cmd.none);
      }
    in Tea_app.program debugged pnode flags


let program :
  ('flags, 'model, 'msg) Tea_app.program ->
  ('msg -> string) ->
  Web.Node.t Js.null_undefined ->
  'flags ->
  'msg debug_msg Tea_app.programInterface
  = fun { init; update; view; subscriptions; shutdown } string_of_msg pnode flags ->
    let debugged = debug_program
      string_of_msg
      {
        init;
        update;
        view;
        subscriptions;
        shutdown;
      }
    in Tea_app.program debugged pnode flags


let navigationProgram :
  (Web.Location.location -> 'msg) ->
  ('flags, 'model, 'msg) Tea_navigation.navigationProgram ->
  ('msg -> string) ->
  Web.Node.t Js.null_undefined ->
  'flags ->
  'msg debug_msg Tea_app.programInterface
  = fun location_to_msg { init; update; view; subscriptions; shutdown } string_of_msg pnode flags ->
  let location = fun location ->
    location
    |> location_to_msg
    |> client_msg
  in
  let debugged = debug_navigation_progam
    string_of_msg
    {
      init;
      update;
      view;
      subscriptions;
      shutdown;
    }
  in Tea_navigation.navigationProgram location debugged pnode flags
