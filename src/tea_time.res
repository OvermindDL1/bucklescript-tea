type t = float

let every = (~key, interval: float, tagger) => {
  open Vdom
  let enableCall = callbacks => {
    let id = Js.Global.setIntervalFloat(() => callbacks.enqueue(tagger(Js.Date.now())), interval)
    () => Js.Global.clearInterval(id)
  }
  Tea_sub.registration(key, enableCall)
}

let delay = (msTime: float, msg) =>
  Tea_cmd.call(callbacks => {
    let _unhandledID = Js.Global.setTimeoutFloat(() => {
      callbacks.contents.enqueue(msg)
    }, msTime)
  })

/* Generic Helpers */

let millisecond = 1.0

let second = 1000.0 *. millisecond

let minute = 60.0 *. second

let hour = 60.0 *. minute

let inMilliseconds = t => t

let inSeconds = t => t /. second

let inMinutes = t => t /. minute

let inHours = t => t /. hour
