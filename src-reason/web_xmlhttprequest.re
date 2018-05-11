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
  [@bs]
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
    pub onloadend: event_loadend => unit
  };
  /* Methods */

type t = Js.t(_xmlhttprequest);

[@bs.new] external create : unit => t = "XMLHttpRequest";

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
let abort = x => x##abort();

let getAllResponseHeaders = x =>
  Tea_result.(
    switch (Js.Null.toOption(x##getAllResponseHeaders())) {
    | None => Error(IncompleteResponse)
    | Some("") => Error(NetworkError)
    | Some(s) => Ok(s)
    }
  );

let getAllResponseHeadersAsList = x =>
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

let getAllResponseHeadersAsDict = x => {
  module StringMap = Map.Make(String);
  switch (getAllResponseHeadersAsList(x)) {
  | Tea_result.Error(_) as err => err
  | Tea_result.Ok(l) =>
    let insert = (d, (k, v)) => StringMap.add(k, v, d);
    Tea_result.Ok(List.fold_left(insert, StringMap.empty, l));
  };
};

let getResponseHeader = (key, x) => Js.Null.toOption(x##getResponse(key));

let open_ = (method', url, ~async=true, ~user="", ~password="", x) =>
  x##_open(method', url, async, user, password);

let overrideMimeType = (mimetype, x) => x##overrideMimeType(mimetype);

let send = (body, x) =>
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
let setRequestHeader = (header, value, x) =>
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

let set_onreadystatechange = (cb, x) => x##onreadystatechange#=cb;

let get_onreadystatechange = x => x##onreadystatechange;

let readyState = x =>
  switch (x##readystate) {
  | 0 => Unsent
  | 1 => Opened
  | 2 => HeadersReceived
  | 3 => Loading
  | 4 => Done
  | i =>
    failwith("Invalid return from 'readystate' of: " ++ string_of_int(i))
  };

let set_responseType = (typ, x) =>
  switch (typ) {
  | StringResponseType => x##responseType#=""
  | ArrayBufferResponseType => x##responseType#="arraybuffer"
  | BlobResponseType => x##responseType#="blob"
  | DocumentResponseType => x##responseType#="document"
  | JsonResponseType => x##responseType#="json"
  | TextResponseType => x##responseType#="text"
  | RawResponseType(s) => x##responseType#=s
  };

let get_responseType = x =>
  switch (x##responseType) {
  | "" => StringResponseType
  | "arraybuffer" => ArrayBufferResponseType
  | "blob" => BlobResponseType
  | "document" => DocumentResponseType
  | "json" => JsonResponseType
  | "text" => TextResponseType
  | s => RawResponseType(s)
  };

let get_response = x =>
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

let get_responseText = x => x##responseText;

let get_responseURL = x => x##responseURL;

let get_responseXML = x => Js.Null.toOption(x##responseXML);

let get_status = x => x##status;

let get_statusText = x => x##statusText;

let set_timeout = (t, x) => x##timeout#=t;

let get_timeout = x => x##timeout;

let set_withCredentials = (b, x) => x##withCredentials#=b;

let get_withCredentials = x => x##withCredentials;

let set_onabort = (cb, x) => x##onabort#=cb;

let get_onabort = x => x##onabort;

let set_onerror = (cb, x) => x##onerror#=cb;

let get_onerror = x => x##onerror;

let set_onload = (cb, x) => x##onload#=cb;

let get_onload = x => x##onload;

let set_onloadstart = (cb, x) => x##onloadstart#=cb;

let get_onloadstart = x => x##onloadstart;

let set_onprogress = (cb, x) => x##onprogress#=cb;

let get_onprogress = x => x##onprogress;

let set_ontimeout = (cb, x) => x##ontimeout#=cb;

let get_ontimeout = x => x##ontimeout;

let set_onloadend = (cb, x) => x##onloadend#=cb;

let get_onloadend = x => x##onloadend;
