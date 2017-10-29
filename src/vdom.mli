(* https://github.com/Matt-Esch/virtual-dom/blob/master/docs/vnode.md *)

type 'msg applicationCallbacks = {
  enqueue : 'msg -> unit;
}

(* Attributes are not properties *)
(* https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes *)

type 'msg eventHandler =
  | EventHandlerCallback of string * (Web.Node.event -> 'msg option)
  | EventHandlerMsg of 'msg

type 'msg eventCache =
  { handler : Web.Node.event_cb
  ; cb : (Web.Node.event -> 'msg option) ref
  }

type 'msg property =
  | NoProp
  | RawProp of string * string (* TODO:  This last string needs to be made something more generic, maybe a function... *)
  (* Attribute (namespace, key, value) *)
  | Attribute of string * string * string
  | Data of string * string
  (* Event (name, userkey, callback) *)
  | Event of string * 'msg eventHandler * 'msg eventCache option ref
  (* | Event of string * (Web.Event.t -> 'msg) *)
  | Style of (string * string) list

type 'msg properties = 'msg property list

type 'msg t =
  | CommentNode of string
  | Text of string
  (* Node (namespace, tagName, key, unique, properties, children)  *)
  | Node of string * string * string * string * 'msg properties * 'msg t list
  (* | ArrayNode of string * string * string * string * 'msg property array * 'msg t array *)
  (* LazyGen (key, fnGenerator) *)
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  (* Tagger (toString, toDom, toVNodes) *)
(* | Tagger of (unit -> string) * ('msg applicationCallbacks ref -> Web.Node.t -> Web.Node.t -> int ->  'msg t list -> Web.Node.t) * (unit -> 'msg t) *)
  (* Tagger (tagger, vdom) *)
  | Tagger of ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t
  (*  *)
  (* | Tagger of (('a -> 'msg) -> 'a t -> 'msg t) *)
  (* Custom (key, cbAdd, cbRemove, cbChange, properties, children) *)
  (* | Custom of string * (unit -> Web.Node.t) * (Web.Node.t -> unit) * *)


(* Nodes *)

val noNode : 'msg t

val comment : string -> 'msg t

val text : string -> 'msg t

val fullnode : string -> string -> string -> string
  -> 'msg properties -> 'msg t list -> 'msg t

val node : ?namespace:string -> string -> ?key:string -> ?unique:string
  -> 'msg properties -> 'msg t list -> 'msg t

val lazyGen : string -> (unit -> 'msg t) -> 'msg t

(* Properties *)

val noProp : 'msg property

val prop : string -> string -> 'msg property

val onCB : string -> string -> (Web.Node.event -> 'msg option) ->
  'msg property

val onMsg : string -> 'msg -> 'msg property

val attribute : string -> string -> string -> 'msg property

val data : string -> string -> 'msg property

val style : string -> string -> 'msg property

val styles : (string * string) list -> 'msg property

(* Accessors *)

val renderToHtmlString : 'msg t -> string

(* Patching / Diffing *)

val eventHandler_Register :
  'msg applicationCallbacks ref ->
    Web.Node.t ->
      string ->
        'msg eventHandler ->
          'msg eventCache option

val eventHandler_Unregister : Web.Node.t -> string
  -> 'msg eventCache option -> 'msg option


val patchVNodesIntoElement : 'msg applicationCallbacks ref ->
  Web.Node.t -> 'msg t list -> 'msg t list -> 'msg t list

(* TODO: rename mapCallbacks *)
val wrapCallbacks : ('a -> 'b) -> 'b applicationCallbacks ref ->
  'a applicationCallbacks ref

val map : ('a -> 'b) -> 'a t -> 'b t
