type t = float;

/* type 'msg mySub =
     | Every of t * (t -> 'msg)


   type 'msg myCmd =
     | Delay of t * (unit -> 'msg) */

let every = (~key, interval, tagger) => {
  open Vdom;
  let enableCall = callbacks => {
    let id =
      Web.Window.setInterval(
        () => callbacks.enqueue(tagger(Web.Date.now())),
        interval,
      );
    /* let () = Js.log ("Time.every", "enable", interval, tagger, callbacks) in */
    () =>
      /* let () = Js.log ("Time.every", "disable", id, interval, tagger, callbacks) in */
      Web.Window.clearTimeout(id);
  };
  Tea_sub.registration(key, enableCall);
};

let delay = (msTime, msg) =>
  Tea_cmd.call(callbacks => {
    let _unhandledID =
      Web.Window.setTimeout(() => Vdom.(callbacks^.enqueue(msg)), msTime);
    ();
  });

/* Generic Helpers */

let millisecond = 1.0;

let second = 1000.0 *. millisecond;

let minute = 60.0 *. second;

let hour = 60.0 *. minute;

let inMilliseconds = t => t;

let inSeconds = t => t /. second;

let inMinutes = t => t /. minute;

let inHours = t => t /. hour;
