type navigationProgram('flags, 'model, 'msg) = {
  init: ('flags, Web.Location.location) => ('model, Tea_cmd.t('msg)),
  update: ('model, 'msg) => ('model, Tea_cmd.t('msg)),
  view: 'model => Vdom.t('msg),
  subscriptions: 'model => Tea_sub.t('msg),
  shutdown: 'model => Tea_cmd.t('msg),
};

let getLocation = () => Web.Location.asRecord(Web.Document.location());

let notifier: ref(option(Web.Location.location => unit)) = ref(None);

let notifyUrlChange = () =>
  switch (notifier^) {
  | None => ()
  | Some(cb) =>
    let location = getLocation();
    let () = cb(location);
    ();
  };

let subscribe = tagger => {
  open Vdom;
  let enableCall = callbacks => {
    let notifyHandler = location => callbacks.enqueue(tagger(location));
    let () = notifier := Some(notifyHandler);
    let handler: Web.Node.event_cb = (. _event) => notifyUrlChange();
    let () = Web.Window.addEventListener("popstate", handler, false);
    () => Web.Window.removeEventListener("popstate", handler, false);
  };
  Tea_sub.registration("navigation", enableCall);
};

let replaceState = url => {
  let _ =
    Web.Window.History.replaceState(
      Web.Window.window,
      Js.Json.parseExn("{}"),
      "",
      url,
    );
  ();
};

let pushState = url => {
  let _ =
    Web.Window.History.pushState(
      Web.Window.window,
      Js.Json.parseExn("{}"),
      "",
      url,
    );
  ();
};

let modifyUrl = url =>
  Tea_cmd.call(_enqueue => {
    let () = replaceState(url);
    let () = notifyUrlChange();
    ();
  });

let newUrl = url =>
  Tea_cmd.call(_enqueue => {
    let () = pushState(url);
    let () = notifyUrlChange();
    ();
  });

let navigationProgram = (locationToMessage, stuff) => {
  let init = flag => stuff.init(flag, getLocation());
  let subscriptions = model =>
    Tea_sub.batch([
      subscribe(locationToMessage),
      stuff.subscriptions(model),
    ]);
  open! Tea_app;
  program({
    init,
    update: stuff.update,
    view: stuff.view,
    subscriptions,
    shutdown: stuff.shutdown,
  });
};
