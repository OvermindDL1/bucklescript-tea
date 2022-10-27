
type unresolved

type xmlHttpRequestUpload

type event_readystatechange = Web_json.t
type event_abort = Web_json.t
type event_error = Web_json.t
type event_load = Web_json.t
type event_loadstart = Web_json.t
type event_progress = Web_json.t
type event_timeout = Web_json.t
type event_loadend = Web_json.t

class type _xmlhttprequest = object
  (* Methods *)
  method abort : unit -> unit
  method getAllResponseHeaders : unit -> string Js.null
  method getResponseHeader : string -> string Js.null
  method _open : string -> string -> bool -> string -> string -> unit
  method overrideMimeType : string -> unit
  method send : unit -> unit
  method send__string : string Js.null -> unit
  method send__formdata : Web_formdata.t -> unit
  method send__document : Web_document.t -> unit
  (* method send_blob : Web_blob.t -> unit *)
  (* method send_arrayBufferView : Web_arraybuffer_view.t -> unit *)
  method setRequestHeader : string -> string -> unit

  (* Properties *)
  method onreadystatechange : (event_readystatechange -> unit) [@@bs.get] [@@bs.set]
  method readyState : int [@@bs.get]
  method responseType : string [@@bs.get] [@@bs.set]
  method response : unresolved Js.null [@@bs.get]
  method responseText : string [@@bs.get]
  method responseURL : string [@@bs.get]
  method responseXML : Web_document.t Js.null [@@bs.get]
  method status : int [@@bs.get]
  method statusText : string [@@bs.get]
  method timeout : float [@@bs.get] [@@bs.set]
  method upload : xmlHttpRequestUpload [@@bs.get]
  method withCredentials : bool [@@bs.get] [@@bs.set]

  (* Base events *)
  method onabort : (event_abort -> unit) [@@bs.get] [@@bs.set]
  method onerror : (event_error -> unit) [@@bs.get] [@@bs.set]
  method onload : (event_load -> unit) [@@bs.get] [@@bs.set]
  method onloadstart : (event_loadstart -> unit) [@@bs.get] [@@bs.set]
  method onprogress : (event_loadstart -> unit) [@@bs.get] [@@bs.set]
  method ontimeout : (event_timeout -> unit) [@@bs.get] [@@bs.set]
  method onloadend : (event_loadend -> unit) [@@bs.get] [@@bs.set]
end [@bs]
type t = _xmlhttprequest Js.t

external create : unit -> t = "XMLHttpRequest" [@@bs.new]

type errors =
  | IncompleteResponse
  | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody of string
  | FormDataBody of Web_formdata.t
  | FormListBody of (string * string) list
  | DocumentBody of Web_document.t
  (* | BlobBody of Web_blob.t *)
  (* | ArrayBufferViewBody of Web_arraybuffer_view.t *)

(* Main interface functions *)

let abort (x: t) : unit = x##abort ()

let getAllResponseHeaders (x: t) : (string, errors) Tea_result.t =
  let open Tea_result in
  match Js.Null.toOption (x##getAllResponseHeaders ()) with
  | None -> Error IncompleteResponse
  | Some "" -> Error NetworkError
  | Some s -> Ok s

let getAllResponseHeadersAsList (x: t) : ((string * string) list, errors) Tea_result.t =
  let open Tea_result in
  match getAllResponseHeaders x with
  | Error _ as err -> err
  | Ok s -> Ok
    ( s
      |> Js.String.split "\r\n"
      |> Array.map (Js.String.splitAtMost ": " ~limit:2)
      |> Array.to_list
      |> List.filter (fun a -> Array.length a == 2)
      |> List.map
        ( function
          | [|key; value|] -> (key, value)
          | _ -> failwith "Cannot happen, already checked length"
        )
    )

let getAllResponseHeadersAsDict (x: t) : (string Map.Make(String).t, errors) Tea_result.t =
  let module StringMap = Map.Make(String) in
  match getAllResponseHeadersAsList x with
  | Tea_result.Error _ as err -> err
  | Tea_result.Ok l ->
    let insert d (k, v) = StringMap.add k v d in
    Tea_result.Ok (List.fold_left insert StringMap.empty l)

let getResponseHeader key x = Js.Null.toOption (x##getResponse key)

let open_ (method': string) (url: string) ?(async=true) ?(user="") ?(password="") x =
  x##_open method' url async user password

let overrideMimeType (mimetype: string) (x: t) : unit =
  x##overrideMimeType mimetype

let send (body: body) (x: t) : unit =
  match body with
  | EmptyBody -> x##send ()
  | EmptyStringBody -> x##send__string Js.Null.empty
  | StringBody s -> x##send__string (Js.Null.return s)
  | FormDataBody f -> x##send__formdata f
  | FormListBody l ->
    let form =
      List.fold_left
        (fun f (key, value) -> let () = Web_formdata.append key value f in f)
        (Web_formdata.create ())
        l in
    x##send__formdata form
  | DocumentBody d -> x##send__document d
  (* | BlobBody b -> x##send_blob b *)
  (* | ArrayBufferViewBody a -> x##send_arrayBufferView a *)

let setRequestHeader (header: string) (value: string) (x: t) =
  x##setRequestHeader header value


(* Properties *)

type state =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done

type responseType =
  | StringResponseType
  | ArrayBufferResponseType
  | BlobResponseType
  | DocumentResponseType
  | JsonResponseType
  | TextResponseType
  | RawResponseType of string

type responseBody =
  | NoResponse
  | StringResponse of string
  | ArrayBufferResponse of unit
  | BlobResponse of unit
  | DocumentResponse of Web_document.t
  | JsonResponse of Web_json.t
  | TextResponse of string
  | RawResponse of string * unit

let set_onreadystatechange (cb: event_readystatechange -> unit) (x: t) : unit =
  x##onreadystatechange #= cb

let get_onreadystatechange (x: t) : (event_readystatechange -> unit) =
  x##onreadystatechange

let readyState (x: t) : state =
  match x##readyState with
  | 0 -> Unsent
  | 1 -> Opened
  | 2 -> HeadersReceived
  | 3 -> Loading
  | 4 -> Done
  | i -> failwith ("Invalid return from 'readystate' of: " ^ string_of_int i)

let set_responseType (typ: responseType) (x: t) : unit =
  match typ with
  | StringResponseType -> x##responseType #= ""
  | ArrayBufferResponseType -> x##responseType #= "arraybuffer"
  | BlobResponseType -> x##responseType #= "blob"
  | DocumentResponseType -> x##responseType #= "document"
  | JsonResponseType -> x##responseType #= "json"
  | TextResponseType -> x##responseType #= "text"
  | RawResponseType s -> x##responseType #= s

let get_responseType (x: t) : responseType =
  match x##responseType with
  | "" -> StringResponseType
  | "arraybuffer" -> ArrayBufferResponseType
  | "blob" -> BlobResponseType
  | "document" -> DocumentResponseType
  | "json" -> JsonResponseType
  | "text" -> TextResponseType
  | s -> RawResponseType s

let get_response (x: t) : responseBody =
  match Js.Null.toOption x##response with
  | None -> NoResponse
  | Some resp ->
    match get_responseType x with
    | StringResponseType -> StringResponse (Obj.magic resp)
    | ArrayBufferResponseType -> ArrayBufferResponse (Obj.magic resp)
    | BlobResponseType -> BlobResponse (Obj.magic resp)
    | DocumentResponseType -> DocumentResponse (Obj.magic resp)
    | JsonResponseType -> JsonResponse (Obj.magic resp)
    | TextResponseType -> TextResponse (Obj.magic resp)
    | RawResponseType s -> RawResponse (s, Obj.magic resp)

let get_responseText (x: t) : string = x##responseText

let get_responseURL (x: t) : string = x##responseURL

let get_responseXML (x: t) : Web_document.t option =
  Js.Null.toOption x##responseXML

let get_status (x: t) : int = x##status

let get_statusText (x: t) : string = x##statusText

let set_timeout (t: float) (x: t) : unit =
  x##timeout #= t

let get_timeout (x: t) : float = x##timeout

let set_withCredentials (b: bool) (x: t) : unit =
  x##withCredentials #= b

let get_withCredentials (x: t) : bool = x##withCredentials

let set_onabort (cb: event_abort -> unit) (x: t) : unit =
  x##onabort #= cb

let get_onabort (x: t) : (event_abort -> unit)= x##onabort

let set_onerror (cb: event_error -> unit) (x: t) : unit =
  x##onerror #= cb

let get_onerror (x: t) : (event_error -> unit)= x##onerror

let set_onload (cb: event_load -> unit) (x: t) : unit = x##onload #= cb

let get_onload (x: t) : (event_load -> unit) = x##onload

let set_onloadstart (cb: event_loadstart -> unit) (x: t) : unit =
  x##onloadstart #= cb

let get_onloadstart (x: t) : (event_loadstart -> unit) = x##onloadstart

let set_onprogress (cb: event_loadstart -> unit) (x: t) : unit =
  x##onprogress #= cb

let get_onprogress (x: t) : (event_loadstart -> unit)= x##onprogress

let set_ontimeout (cb: event_timeout -> unit) (x: t) : unit =
  x##ontimeout #= cb

let get_ontimeout (x: t) : (event_timeout -> unit) = x##ontimeout

let set_onloadend (cb: event_loadend -> unit) (x: t) : unit =
  x##onloadend #= cb

let get_onloadend (x: t) : (event_loadend -> unit) = x##onloadend
