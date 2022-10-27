type unresolved;

type xmlHttpRequestUpload;

type event_readystatechange = Web_json.t;
type event_abort = Web_json.t;
type event_error = Web_json.t;
type event_load = Web_json.t;
type event_loadstart = Web_json.t;
type event_progress = Web_json.t;
type event_timeout = Web_json.t;
type event_loadend = Web_json.t;

class type _xmlhttprequest =
  {
    pub abort: unit => unit;
    pub getAllResponseHeaders: unit => Js.null(string);
    pub getResponseHeader: string => Js.null(string);
    pub _open: (string, string, bool, string, string) => unit;
    pub overrideMimeType: string => unit;
    pub send: unit => unit;
    pub send__string: Js.null(string) => unit;
    pub send__formdata: Web_formdata.t => unit;
    pub send__document: Web_document.t => unit;
    /* method send_blob : Web_blob.t -> unit */
    /* method send_arrayBufferView : Web_arraybuffer_view.t -> unit */
    pub setRequestHeader: (string, string) => unit;
    /* Properties */
    [@bs.get]
    [@bs.set]
    pub onreadystatechange: event_readystatechange => unit;
    [@bs.get]
    pub readyState: int;
    [@bs.get]
    [@bs.set]
    pub responseType: string;
    [@bs.get]
    pub response: Js.null(unresolved);
    [@bs.get]
    pub responseText: string;
    [@bs.get]
    pub responseURL: string;
    [@bs.get]
    pub responseXML: Js.null(Web_document.t);
    [@bs.get]
    pub status: int;
    [@bs.get]
    pub statusText: string;
    [@bs.get]
    [@bs.set]
    pub timeout: float;
    [@bs.get]
    pub upload: xmlHttpRequestUpload;
    [@bs.get]
    [@bs.set]
    pub withCredentials: bool;
    /* Base events */
    [@bs.get]
    [@bs.set]
    pub onabort: event_abort => unit;
    [@bs.get]
    [@bs.set]
    pub onerror: event_error => unit;
    [@bs.get]
    [@bs.set]
    pub onload: event_load => unit;
    [@bs.get]
    [@bs.set]
    pub onloadstart: event_loadstart => unit;
    [@bs.get]
    [@bs.set]
    pub onprogress: event_loadstart => unit;
    [@bs.get]
    [@bs.set]
    pub ontimeout: event_timeout => unit;
    [@bs.get]
    [@bs.set]
    pub onloadend: event_loadend => unit;
  };
  /* Methods */
type t = Js.t(_xmlhttprequest);

[@bs.new] external create: unit => t = "XMLHttpRequest";

type errors =
  | IncompleteResponse
  | NetworkError;

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody(string)
  | FormDataBody(Web_formdata.t)
  | FormListBody(list((string, string)))
  | DocumentBody(Web_document.t);
/* | BlobBody of Web_blob.t */
/* | ArrayBufferViewBody of Web_arraybuffer_view.t */

/* Main interface functions */

let abort = (x: t): unit => x##abort();

let getAllResponseHeaders = (x: t): Tea_result.t(string, errors) =>
  Tea_result.(
    switch (Js.Null.toOption(x##getAllResponseHeaders())) {
    | None => Error(IncompleteResponse)
    | Some("") => Error(NetworkError)
    | Some(s) => Ok(s)
    }
  );

let getAllResponseHeadersAsList =
    (x: t): Tea_result.t(list((string, string)), errors) =>
  Tea_result.(
    switch (getAllResponseHeaders(x)) {
    | Error(_) as err => err
    | Ok(s) =>
      Ok(
        s
        |> Js.String.split("\r\n")
        |> Array.map(Js.String.splitAtMost(": ", ~limit=2))
        |> Array.to_list
        |> List.filter(a => Array.length(a) === 2)
        |> List.map(
             fun
             | [|key, value|] => (key, value)
             | _ => failwith("Cannot happen, already checked length"),
           ),
      )
    }
  );

let getAllResponseHeadersAsDict =
    (x: t): Tea_result.t(Map.Make(String).t(string), errors) => {
  module StringMap = Map.Make(String);
  switch (getAllResponseHeadersAsList(x)) {
  | Tea_result.Error(_) as err => err
  | Tea_result.Ok(l) =>
    let insert = (d, (k, v)) => StringMap.add(k, v, d);
    Tea_result.Ok(List.fold_left(insert, StringMap.empty, l));
  };
};

let getResponseHeader = (key, x) => Js.Null.toOption(x##getResponse(key));

let open_ =
    (method': string, url: string, ~async=true, ~user="", ~password="", x) =>
  x##_open(method', url, async, user, password);

let overrideMimeType = (mimetype: string, x: t): unit =>
  x##overrideMimeType(mimetype);

let send = (body: body, x: t): unit =>
  switch (body) {
  | EmptyBody => x##send()
  | EmptyStringBody => x##send__string(Js.Null.empty)
  | StringBody(s) => x##send__string(Js.Null.return(s))
  | FormDataBody(f) => x##send__formdata(f)
  | FormListBody(l) =>
    let form =
      List.fold_left(
        (f, (key, value)) => {
          let () = Web_formdata.append(key, value, f);
          f;
        },
        Web_formdata.create(),
        l,
      );
    x##send__formdata(form);
  | DocumentBody(d) => x##send__document(d)
  };
/* | BlobBody b -> x##send_blob b */
/* | ArrayBufferViewBody a -> x##send_arrayBufferView a */

let setRequestHeader = (header: string, value: string, x: t) =>
  x##setRequestHeader(header, value);

/* Properties */

type state =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done;

type responseType =
  | StringResponseType
  | ArrayBufferResponseType
  | BlobResponseType
  | DocumentResponseType
  | JsonResponseType
  | TextResponseType
  | RawResponseType(string);

type responseBody =
  | NoResponse
  | StringResponse(string)
  | ArrayBufferResponse(unit)
  | BlobResponse(unit)
  | DocumentResponse(Web_document.t)
  | JsonResponse(Web_json.t)
  | TextResponse(string)
  | RawResponse(string, unit);

let set_onreadystatechange = (cb: event_readystatechange => unit, x: t): unit =>
  x##onreadystatechange #= cb;

let get_onreadystatechange = (x: t): (event_readystatechange => unit) =>
  x##onreadystatechange;

let readyState = (x: t): state =>
  switch (x##readyState) {
  | 0 => Unsent
  | 1 => Opened
  | 2 => HeadersReceived
  | 3 => Loading
  | 4 => Done
  | i =>
    failwith("Invalid return from 'readystate' of: " ++ string_of_int(i))
  };

let set_responseType = (typ: responseType, x: t): unit =>
  switch (typ) {
  | StringResponseType => x##responseType #= ""
  | ArrayBufferResponseType => x##responseType #= "arraybuffer"
  | BlobResponseType => x##responseType #= "blob"
  | DocumentResponseType => x##responseType #= "document"
  | JsonResponseType => x##responseType #= "json"
  | TextResponseType => x##responseType #= "text"
  | RawResponseType(s) => x##responseType #= s
  };

let get_responseType = (x: t): responseType =>
  switch (x##responseType) {
  | "" => StringResponseType
  | "arraybuffer" => ArrayBufferResponseType
  | "blob" => BlobResponseType
  | "document" => DocumentResponseType
  | "json" => JsonResponseType
  | "text" => TextResponseType
  | s => RawResponseType(s)
  };

let get_response = (x: t): responseBody =>
  switch (Js.Null.toOption(x##response)) {
  | None => NoResponse
  | Some(resp) =>
    switch (get_responseType(x)) {
    | StringResponseType => StringResponse(Obj.magic(resp))
    | ArrayBufferResponseType => ArrayBufferResponse(Obj.magic(resp))
    | BlobResponseType => BlobResponse(Obj.magic(resp))
    | DocumentResponseType => DocumentResponse(Obj.magic(resp))
    | JsonResponseType => JsonResponse(Obj.magic(resp))
    | TextResponseType => TextResponse(Obj.magic(resp))
    | RawResponseType(s) => [@implicit_arity] RawResponse(s, Obj.magic(resp))
    }
  };

let get_responseText = (x: t): string => x##responseText;

let get_responseURL = (x: t): string => x##responseURL;

let get_responseXML = (x: t): option(Web_document.t) =>
  Js.Null.toOption(x##responseXML);

let get_status = (x: t): int => x##status;

let get_statusText = (x: t): string => x##statusText;

let set_timeout = (t: float, x: t): unit => x##timeout #= t;

let get_timeout = (x: t): float => x##timeout;

let set_withCredentials = (b: bool, x: t): unit => x##withCredentials #= b;

let get_withCredentials = (x: t): bool => x##withCredentials;

let set_onabort = (cb: event_abort => unit, x: t): unit => x##onabort #= cb;

let get_onabort = (x: t): (event_abort => unit) => x##onabort;

let set_onerror = (cb: event_error => unit, x: t): unit => x##onerror #= cb;

let get_onerror = (x: t): (event_error => unit) => x##onerror;

let set_onload = (cb: event_load => unit, x: t): unit => x##onload #= cb;

let get_onload = (x: t): (event_load => unit) => x##onload;

let set_onloadstart = (cb: event_loadstart => unit, x: t): unit =>
  x##onloadstart #= cb;

let get_onloadstart = (x: t): (event_loadstart => unit) => x##onloadstart;

let set_onprogress = (cb: event_loadstart => unit, x: t): unit =>
  x##onprogress #= cb;

let get_onprogress = (x: t): (event_loadstart => unit) => x##onprogress;

let set_ontimeout = (cb: event_timeout => unit, x: t): unit =>
  x##ontimeout #= cb;

let get_ontimeout = (x: t): (event_timeout => unit) => x##ontimeout;

let set_onloadend = (cb: event_loadend => unit, x: t): unit =>
  x##onloadend #= cb;

let get_onloadend = (x: t): (event_loadend => unit) => x##onloadend;
