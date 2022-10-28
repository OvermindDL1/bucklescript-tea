type t = Dom.event

type cb = t -> unit

type options = bool (* false | true (* TODO:  Define a javascript record as another option *) *)


type popstateEvent = <
> Js.t

type popstateCb = popstateEvent -> unit [@bs]
