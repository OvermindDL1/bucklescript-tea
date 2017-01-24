

type t = <
  length : int [@bs.get];
  back : unit -> unit [@bs.meth];
  forward : unit -> unit [@bs.meth];
  go : int -> unit [@bs.meth];
  pushState : Js.Json.t -> string -> string -> unit [@bs.meth];
  replaceState : Js.Json.t -> string -> string -> unit [@bs.meth];
  state : Js.Json.t [@bs.get];
> Js.t


let length window = window##history##length

let back window = window##history##back

let forward window = window##history##forward

let go window to' = window##history##go to'

let pushState window state title url = window##history##pushState state title url

let replaceState window state title url = window##history##replaceState state title url

let state window = window##history##state
