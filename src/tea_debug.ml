module type StandardProgramType = sig
  type cflags
  type cmodel
  type cmsg
  val init : cflags -> cmodel * cmsg Tea_cmd.t
  val update : cmodel -> cmsg -> cmodel * cmsg Tea_cmd.t
  val view : cmodel -> cmsg Vdom.t
  val subscriptions : cmodel -> cmsg Tea_sub.t
  val string_of_msg : cmsg -> string
end

module MakeStandardProgram (Client : StandardProgramType) : sig
  type msg
  val start : Web.Node.t Js.null_undefined -> Client.cflags -> msg Tea_app.programInterface
end = struct

  type msg =
    | ClientMsg of Client.cmsg
    | TogglePaused
    | SelectHistoryItem of int
    | ToggleDetails

  let client_msg msg = ClientMsg msg

  type state =
    | Running
    | Paused of int

  type model = {
    history : (string * Client.cmodel) list;
    state : state;
    show_details : bool;
  }
  [@@bs.deriving {jsConverter}]


  let init flags =
    let cmodel,cmd = Client.init flags in
    {
      history = ["_init_", cmodel];
      state = Running;
      show_details = false;
    }, cmd |> Tea_cmd.map client_msg


  let update model = function
    | ClientMsg msg ->
      if model.state = Running then
        let _,cmodel = List.hd model.history in
        let cmodel',cmd = Client.update cmodel msg in
        let dmodel' = { model with history = (Client.string_of_msg msg, cmodel') :: model.history } in
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


  let view_styles () =
    let open Tea_html in
    let rule selector properties =
      properties |> List.map (fun (k,v) -> k ^ ":" ^ v) |> String.concat ";" |> Printf.sprintf "%s {%s}" selector |> Tea_html.text
    in
    node "style" [] [
      rule "#debug.paused" [
        "position", "fixed"; "top", "0"; "left", "0"; "width", "100%"; "height", "100%"; "pointer-events", "all";
        "background-color", "rgba(0,0,0,.1)";
        "box-shadow", "inset 0 0 10px #333";
      ];
      rule "#debug nav" [
        "position", "fixed"; "bottom", "0"; "right", "6px";
        "border-radius", "4px 4px 0 0"; "background-color", "#444"; "color", "#fff"; "font-family", "monospace";
        "box-shadow", "0 0 10px #333";
      ];
      rule "#debug.paused nav" ["height", "50%"; "padding-bottom", "2em"];
      rule "#debug nav .toggle" ["padding", "6px"; "cursor", "pointer"; "min-width", "24ch"; "text-align", "center"];
      rule "#debug nav .toggle:before" [
        "content", "' '";
        "position", "absolute"; "left", "0"; "top", "0";
        "width", ".5ch"; "height", "1.8ch";
        "margin", ".8ch 1ch";
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
      rule "#debug nav .history li" ["margin", "0"; "padding", "0.2ch"];
      rule "#debug nav .history li.selected" ["background-color", "#333"];
      rule "#debug nav .history span.details" [
        "display", "inline-block"; "cursor", "pointer";
        "width", "1ch";
        "margin", "0 1ch";
        "vertical-align", "super";
      ];
      rule "#debug nav .history li.selected span.details:after" ["content", "'\\2026'"];
      rule "#debug nav .history li.selected span.details.show:before" [
        "position", "absolute"; "content", "' '";
        "border", "solid transparent"; "border-right-color", "#333";
        "border-width", "1.6ch"; "margin-left", "-4.4ch"; "margin-top", "-.3ch";
      ];
      rule "#debug nav .history span.message" [
        "display", "inline-block"; "cursor", "pointer";
        "white-space", "nowrap"; "overflow", "hidden"; "text-overflow", "ellipsis";
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

  let view_details model =
    let open Tea_html in
    let format = [%raw {|
      function (v) { return JSON.stringify(v, null, 2); }
    |}] in
    aside [class' "details"] [model |> format |> text]

  let view_history model selected_index =
    let open Tea_html in
    let count = List.length model.history in
    let title = Vdom.attribute "" "title" in
    ul [class' "history"] @@
      List.mapi (fun i (msg, cmodel) ->
          let selected = i = selected_index in
          li [
            onClick (SelectHistoryItem i);
            classList ["selected", selected]
          ] [
            span (
              classList ["details", true; "show", selected && model.show_details] ::
              if selected
              then [onClick ToggleDetails; title "toggle details"]
              else [noProp;noProp]
            ) [
              if selected && model.show_details
              then view_details cmodel
              else noNode
            ];
            span [class' "message"] [text msg];
            span [class' "index"] [(count - i) |> string_of_int |> text];
          ]
      ) model.history

  let view model =
    let open Tea_html in
    let title = Vdom.attribute "" "title" in
    let selected_index, selected_model, paused = match model.state with
      | Running -> 0, (List.hd model.history |> snd), false
      | Paused index -> index, (List.nth model.history index |> snd), true
    in
    let history_count = List.length model.history in
    div [] [
      Client.view selected_model |> Tea_app.map client_msg;
      div [id "debug"; classList ["paused", paused]] [
        view_styles ();
        nav [] [
          div [
            class' "toggle"; onClick TogglePaused;
            if paused then title "click to resume"
            else title "click to pause"
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


  let subscriptions model =
    let subs = Client.subscriptions (List.hd model.history |> snd) in
    subs |> Tea_sub.map client_msg


  let start =
    Tea_app.standardProgram { init; update; view; subscriptions; }

end
