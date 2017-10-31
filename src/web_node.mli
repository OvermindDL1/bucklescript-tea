type style

val getStyle : style -> string -> string Js.null

val setStyle : style -> string -> string Js.null -> unit

type t

val document_node : t

type event = t Web_event.t

type event_cb = t Web_event.cb

val getProp_asEventListener : t -> 'key -> t Web_event.cb Js.undefined

val setProp_asEventListener : t -> 'key -> t Web_event.cb Js.undefined ->
  unit

val getValue : t -> string Js.Undefined.t
val getChecked : t -> bool Js.Undefined.t

val getProp : t -> 'key -> 'value

val setProp : t -> 'key -> 'value -> unit

val style : t -> style

val getStyle : t -> string -> string Js.null

val setStyle : t -> string -> string Js.null -> unit

val setStyleProperty: t -> ?priority:bool -> string ->
  string Js.null -> unit

val childNodes: t -> t Js.Array.t
val firstChild: t -> t Js.Null.t
val appendChild: t -> t -> t
val removeChild: t -> t -> t
val insertBefore: t -> t -> t -> t
val remove: t -> unit

val setAttributeNS : t -> string -> string -> string -> unit
val setAttribute : t -> string -> string -> unit
val setAttributeNsOptional : t -> string -> string -> string -> unit
val removeAttributeNS : t -> string -> string -> unit
val removeAttribute : t -> string -> unit
val removeAttributeNsOptional : t -> string -> string -> unit

val addEventListener : t -> string -> t Web_event.cb ->
  Web_event.options -> unit

val removeEventListener : t -> string -> t Web_event.cb ->
  Web_event.options -> unit

val focus : t -> unit

(* Text Nodes only *)

val set_nodeValue: t -> string -> unit
val get_nodeValue: t -> string Js.Null.t


(* Polyfills *)
val remove_polyfill : unit -> unit
