type t = {
  time: Tea_time.t,
  delta: Tea_time.t,
}

let every = (~key="", tagger) => {
  open Vdom
  let enableCall = callbacks => {
    let lastTime = ref(Js.Date.now())
    let id = ref(None)
    let rec onFrame = _time => {
      let time = Js.Date.now()
      switch id.contents {
      | None => ()
      | Some(_i) =>
        let ret = {
          time: time,
          delta: if time < lastTime.contents {
            0.0
          } else {
            time -. lastTime.contents
          },
        }
        let () = lastTime := time
        let () = callbacks.enqueue(tagger(ret))
        switch id.contents {
        | None => ()
        | Some(_stillActive) =>
          let () = id := Some(Webapi.requestCancellableAnimationFrame(onFrame))
        }
      }
    }
    let () = id := Some(Webapi.requestCancellableAnimationFrame(onFrame))
    () =>
      switch id.contents {
      | None => ()
      | Some(i) =>
        let () = Webapi.cancelAnimationFrame(i)
        let () = id := None
      }
  }
  Tea_sub.registration(key, enableCall)
}

let times = (~key="", tagger) => every(ev => tagger(~key, ev.time))

let diffs = (~key="", tagger) => every(ev => tagger(~key, ev.delta))
