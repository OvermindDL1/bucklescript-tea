open Vdom

module Cmds = Tea_html_cmds

(* let map lift vdom =
   *)

(* Nodes *)

let noNode = noNode

let text str = text str

let lazy1 key gen = lazyGen key gen

let node ?(namespace="") tagName ?(key="") ?(unique="") props nodes = fullnode namespace tagName key unique props nodes

(* let embedProgram main = custom *)


(* HTML Elements *)

let br props = fullnode "" "br" "br" "br" props []

let br' ?(key="") ?(unique="") props nodes = fullnode "" "br" key unique props nodes

let div ?(key="") ?(unique="") props nodes = fullnode "" "div" key unique props nodes

let span ?(key="") ?(unique="") props nodes = fullnode "" "span" key unique props nodes

let p ?(key="") ?(unique="") props nodes = fullnode "" "p" key unique props nodes

let pre ?(key="") ?(unique="") props nodes = fullnode "" "pre" key unique props nodes

let a ?(key="") ?(unique="") props nodes = fullnode "" "a" key unique props nodes

let section ?(key="") ?(unique="") props nodes = fullnode "" "section" key unique props nodes

let header ?(key="") ?(unique="") props nodes = fullnode "" "header" key unique props nodes

let footer ?(key="") ?(unique="") props nodes = fullnode "" "footer" key unique props nodes

let h1 ?(key="") ?(unique="") props nodes = fullnode "" "h1" key unique props nodes

let strong ?(key="") ?(unique="") props nodes = fullnode "" "strong" key unique props nodes

let button ?(key="") ?(unique="") props nodes = fullnode "" "button" key unique props nodes

let input' ?(key="") ?(unique="") props nodes = fullnode "" "input" key unique props nodes

let textarea ?(key="") ?(unique="") props nodes = fullnode "" "textarea" key unique props nodes

let label ?(key="") ?(unique="") props nodes = fullnode "" "label" key unique props nodes

let ul ?(key="") ?(unique="") props nodes = fullnode "" "ul" key unique props nodes

let ol ?(key="") ?(unique="") props nodes = fullnode "" "ol" key unique props nodes

let li ?(key="") ?(unique="") props nodes = fullnode "" "li" key unique props nodes

let table ?(key="") ?(unique="") props nodes = fullnode "" "table" key unique props nodes

let thead ?(key="") ?(unique="") props nodes = fullnode "" "thead" key unique props nodes

let tfoot ?(key="") ?(unique="") props nodes = fullnode "" "tfoot" key unique props nodes

let tbody ?(key="") ?(unique="") props nodes = fullnode "" "tbody" key unique props nodes

let th ?(key="") ?(unique="") props nodes = fullnode "" "th" key unique props nodes

let tr ?(key="") ?(unique="") props nodes = fullnode "" "tr" key unique props nodes

let td ?(key="") ?(unique="") props nodes = fullnode "" "td" key unique props nodes

let progress ?(key="") ?(unique="") props nodes = fullnode "" "progress" key unique props nodes


(* Properties *)

let id str = prop "id" str

let href str = prop "href" str

let class' name = prop "className" name

let classList classes =
  classes
  |> List.filter (fun (_fst, snd) -> snd)
  |> List.map (fun (fst, _snd) -> fst)
  |> String.concat " "
  |> class'

let type' typ = prop "type" typ

let style key value = style key value

let styles s = styles s

let placeholder str = prop "placeholder" str

let autofocus b = if b then prop "autofocus" "autofocus" else noProp

let value str = prop "value" str

let name str = prop "name" str

let checked b = if b then prop "checked" "checked" else noProp

let for' str = prop "htmlFor" str

let hidden b = if b then prop "hidden" "hidden" else noProp

let target t = prop "target" t


(* Events *)

let onKeyed typ key cb = on typ key cb

let on typ ?(key="") cb = on typ key cb

let onInputOpt ?(key="") msg =
  onKeyed "input" key
    (fun ev ->
       match Js.Undefined.to_opt ev##target with
       | None -> None
       | Some target -> match Js.Undefined.to_opt target##value with
         | None -> None
         | Some value -> msg value
    )
    (* (fun ev -> match _eventGetTargetValue ev with
       | None -> failwith "onInput is not attached to something with a target.value on its event"
       | Some value -> msg value
       ) *)

let onInput ?(key="") msg = onInputOpt ~key:key (fun ev -> Some (msg ev))

let onClick ?(key="") msg =
  onKeyed "click" key (fun _ev -> Some msg)

let onDoubleClick ?(key="") msg =
  onKeyed "dblclick" key (fun _ev -> Some msg)

let onBlur ?(key="") msg =
  onKeyed "blur" key (fun _ev -> Some msg)

let onFocus ?(key="") msg =
  onKeyed "focus" key (fun _ev -> Some msg)

let onCheckOpt ?(key="") msg =
  onKeyed "change" key
    (fun ev ->
       match Js.Undefined.to_opt ev##target with
       | None -> None
       | Some target -> match Js.Undefined.to_opt target##checked with
         | None -> None
         | Some value -> msg value
    )

let onCheck ?(key="") msg = onCheckOpt ~key:key (fun ev -> Some (msg ev))


module Attributes = struct

  let max value = attribute "" "max" value

  let min value = attribute "" "min" value

  let step value = attribute "" "step" value

  let disabled b = if b then attribute "" "disabled" "true" else noProp

end
