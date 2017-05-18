

type t = <
  setItem : string -> string -> unit [@bs.meth];
  getItem : string -> string [@bs.meth];
> Js.t


let setItem window key value = match Js.Undefined.to_opt window##localStorage with
  | None -> ()
  | Some localStorage -> localStorage##setItem key value

let getItem window key = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage -> match localStorage##getItem key with
    | "" -> None
    | res -> Some res
