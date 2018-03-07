type 'msg t = 'msg Vdom.t
type 'msg prop = 'msg Vdom.property

(* Nodes *)

val noNode : 'msg t
val text : string -> 'msg t

type 'msg createNode = ?key:string -> ?unique:string ->
  'msg prop list -> 'msg t list -> 'msg t

(* TODO: needed? *)
val node : ?namespace:string -> string -> 'msg createNode

val lazy1 : string -> ( unit -> 'msg t) -> 'msg t

(* HTML Elements *)

val br : 'msg prop list -> 'msg t

val br'        : 'msg createNode
val div        : 'msg createNode
val span       : 'msg createNode
val p          : 'msg createNode
val pre        : 'msg createNode
val a          : 'msg createNode
val section    : 'msg createNode
val header     : 'msg createNode
val footer     : 'msg createNode
val h1         : 'msg createNode
val h2         : 'msg createNode
val h3         : 'msg createNode
val h4         : 'msg createNode
val h5         : 'msg createNode
val h6         : 'msg createNode
val i          : 'msg createNode
val strong     : 'msg createNode
val button     : 'msg createNode
val input'     : 'msg createNode
val textarea   : 'msg createNode
val label      : 'msg createNode
val ul         : 'msg createNode
val ol         : 'msg createNode
val li         : 'msg createNode
val table      : 'msg createNode
val thead      : 'msg createNode
val tfoot      : 'msg createNode
val tbody      : 'msg createNode
val th         : 'msg createNode
val tr         : 'msg createNode
val td         : 'msg createNode
val progress   : 'msg createNode
val img        : 'msg createNode
val select     : 'msg createNode
val option'    : 'msg createNode
val form       : 'msg createNode
val nav        : 'msg createNode
val main       : 'msg createNode
val aside      : 'msg createNode
val article    : 'msg createNode
val details    : 'msg createNode
val figcaption : 'msg createNode
val figure     : 'msg createNode
val mark       : 'msg createNode
val summary    : 'msg createNode
val time       : 'msg createNode

(* Properties *)

val noProp : 'msg prop
val id : string -> 'msg prop

(* `href` is actually an attribute, not a property, but need it here for Elm compat... *)
val href : string -> 'msg prop

(* `src` is actually an attribute, not a property, but need it here for Elm compat... *)
val src : string -> 'msg prop

val class' : string -> 'msg prop
(* val classList : (string * bool) list -> 'msg prop *)
val type' : string -> 'msg prop
val style : string -> string -> 'msg prop
val styles : (string * string) list -> 'msg prop
val placeholder : string -> 'msg prop
val autofocus : bool -> 'msg prop
val value : string -> 'msg prop
val name : string -> 'msg prop
val checked : bool -> 'msg prop
val for' : string -> 'msg prop
val hidden : bool -> 'msg prop
val target : string -> 'msg prop
val action : string -> 'msg prop
val method' : string -> 'msg prop

(* Events *)

(* TODO: should this be exposed? *)
val onCB : string -> string -> (Web.Node.event -> 'msg option) -> 'msg prop
val onMsg : string -> 'msg -> 'msg prop

val onInput : ?key:string -> (string -> 'msg) -> 'msg prop
val onChange : ?key:string -> (string -> 'msg) -> 'msg prop
val onClick : 'msg -> 'msg prop
val onDoubleClick : 'msg -> 'msg prop
val onBlur : 'msg -> 'msg prop
val onFocus : 'msg -> 'msg prop
val onCheck : ?key:string -> (bool -> 'msg) -> 'msg prop
val onMouseDown : 'msg -> 'msg prop
val onMouseUp : 'msg -> 'msg prop
val onMouseEnter : 'msg -> 'msg prop
val onMouseLeave : 'msg -> 'msg prop
val onMouseOver : 'msg -> 'msg prop
val onMouseOut : 'msg -> 'msg prop

module Attributes : sig
  val max : string -> 'msg prop
  val min : string -> 'msg prop
  val step : string -> 'msg prop
  val disabled : bool -> 'msg prop
  val selected : bool -> 'msg prop
  val acceptCharset : string -> 'msg prop
  val rel : string -> 'msg prop
end
