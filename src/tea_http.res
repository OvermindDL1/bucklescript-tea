type response_status = {
  code: int,
  message: string,
}

type requestBody = Web.XMLHttpRequest.body
type bodyType = Web.XMLHttpRequest.responseType
type responseBody = Web.XMLHttpRequest.responseBody

type response = {
  url: string,
  status: response_status,
  headers: Belt.Map.String.t<string>,
  body: responseBody,
}

type error<'parsedata> =
  | BadUrl(string)
  | Timeout
  | NetworkError
  | Aborted
  | BadStatus(response)
  | BadPayload('parsedata, response)

let string_of_error = x =>
  switch x {
  | BadUrl(url) => "Bad Url: " ++ url
  | Timeout => "Timeout"
  | NetworkError => "Unknown network error"
  | Aborted => "Request aborted"
  | BadStatus(resp) => "Bad Status: " ++ resp.url
  | BadPayload(_customData, resp) => "Bad Payload: " ++ resp.url
  }

type header = Header(string, string)

type expect<'res> = Expect(bodyType, response => result<'res, string>)

type requestEvents<'msg> = {
  onreadystatechange: option<
    (ref<Vdom.applicationCallbacks<'msg>>, Web.XMLHttpRequest.event_readystatechange) => unit,
  >,
  onprogress: option<
    (ref<Vdom.applicationCallbacks<'msg>>, Web.XMLHttpRequest.event_progress) => unit,
  >,
}

let emptyRequestEvents = {
  onreadystatechange: None,
  onprogress: None,
}

type rawRequest<'res> = {
  method': string,
  headers: list<header>,
  url: string,
  body: requestBody,
  expect: expect<'res>,
  timeout: option<Tea_time.t>,
  withCredentials: bool,
}

type request<'msg, 'res> = Request(rawRequest<'res>, option<requestEvents<'msg>>)

let expectStringResponse = func => {
  open Web.XMLHttpRequest
  Expect(
    TextResponseType,
    ({body, _}) =>
      switch body {
      | TextResponse(s) => func(s)
      | _ => Error("Non-text response returned")
      },
  )
}

let expectString = expectStringResponse(resString => Ok(resString))

let request = rawRequest => Request(rawRequest, None)

let getString = url =>
  request({
    method': "GET",
    headers: list{},
    url: url,
    body: Web.XMLHttpRequest.EmptyBody,
    expect: expectString,
    timeout: None,
    withCredentials: false,
  })

let toTask = (Request(request, _maybeEvents)) => {
  module StringMap = Belt.Map.String
  let {method', headers, url, body, expect, timeout, withCredentials} = request
  let Expect(typ, responseToResult) = expect
  Tea_task.nativeBinding(cb => {
    let enqRes = (result, _ev) => cb(result)
    let enqResError = result => enqRes(Error(result))
    let enqResOk = result => enqRes(Ok(result))
    let xhr = Web.XMLHttpRequest.create()
    let setEvent = (ev, cb) => ev(cb, xhr)
    let () = setEvent(Web.XMLHttpRequest.set_onerror, enqResError(NetworkError))
    let () = setEvent(Web.XMLHttpRequest.set_ontimeout, enqResError(Timeout))
    let () = setEvent(Web.XMLHttpRequest.set_onabort, enqResError(Aborted))
    let () = setEvent(Web.XMLHttpRequest.set_onload, _ev => {
      open Web.XMLHttpRequest
      let headers = switch getAllResponseHeadersAsDict(xhr) {
      | Error(_e) => StringMap.empty
      | Ok(headers) => headers
      }
      let response = {
        status: {code: get_status(xhr), message: get_statusText(xhr)},
        headers: headers,
        url: get_responseURL(xhr),
        body: get_response(xhr),
      }
      if response.status.code < 200 || 300 <= response.status.code {
        enqResError(BadStatus(response), ())
      } else {
        switch responseToResult(response) {
        | Error(error) => enqResError(BadPayload(error, response), ())
        | Ok(result) => enqResOk(result, ())
        }
      }
    })
    let () = try Web.XMLHttpRequest.open_(method', url, xhr) catch {
    | _ => enqResError(BadUrl(url), ())
    }
    let () = {
      let setHeader = (Header(k, v)) => Web.XMLHttpRequest.setRequestHeader(k, v, xhr)
      let () = List.iter(setHeader, headers)
      let () = Web.XMLHttpRequest.set_responseType(typ, xhr)
      let () = switch timeout {
      | None => ()
      | Some(t) => Web.XMLHttpRequest.set_timeout(t, xhr)
      }
      let () = Web.XMLHttpRequest.set_withCredentials(withCredentials, xhr)
    }
    let () = Web.XMLHttpRequest.send(body, xhr)
  })
}

let send = (resultToMessage, Request(request, maybeEvents)) => {
  module StringMap = Belt.Map.String
  let {method', headers, url, body, expect, timeout, withCredentials} = request
  let Expect(typ, responseToResult) = expect
  Tea_cmd.call(callbacks => {
    let enqRes = (result, _ev) => {
      open Vdom
      callbacks.contents.enqueue(resultToMessage(result))
    }
    let enqResError = result => enqRes(Error(result))
    let enqResOk = result => enqRes(Ok(result))
    let xhr = Web.XMLHttpRequest.create()
    let setEvent = (ev, cb) => ev(cb, xhr)
    let () = switch maybeEvents {
    | None => ()
    | Some({onprogress, onreadystatechange}) =>
      open Web.XMLHttpRequest
      let mayCB = (thenDo, x) =>
        switch x {
        | None => ()
        | Some(v) => thenDo(v(callbacks))
        }
      let () = mayCB(setEvent(set_onreadystatechange), onreadystatechange)
      let () = mayCB(setEvent(set_onprogress), onprogress)
    }
    let () = setEvent(Web.XMLHttpRequest.set_onerror, enqResError(NetworkError))
    let () = setEvent(Web.XMLHttpRequest.set_ontimeout, enqResError(Timeout))
    let () = setEvent(Web.XMLHttpRequest.set_onabort, enqResError(Aborted))
    let () = setEvent(Web.XMLHttpRequest.set_onload, _ev => {
      open Web.XMLHttpRequest
      let headers = switch getAllResponseHeadersAsDict(xhr) {
      | Error(_e) => StringMap.empty
      | Ok(headers) => headers
      }
      let response = {
        status: {code: get_status(xhr), message: get_statusText(xhr)},
        headers: headers,
        url: get_responseURL(xhr),
        body: get_response(xhr),
      }
      if response.status.code < 200 || 300 <= response.status.code {
        enqResError(BadStatus(response), ())
      } else {
        switch responseToResult(response) {
        | Error(error) => enqResError(BadPayload(error, response), ())
        | Ok(result) => enqResOk(result, ())
        }
      }
    })
    let () = try Web.XMLHttpRequest.open_(method', url, xhr) catch {
    | _ => enqResError(BadUrl(url), ())
    }
    let () = {
      let setHeader = (Header(k, v)) => Web.XMLHttpRequest.setRequestHeader(k, v, xhr)
      let () = List.iter(setHeader, headers)
      let () = Web.XMLHttpRequest.set_responseType(typ, xhr)
      let () = switch timeout {
      | None => ()
      | Some(t) => Web.XMLHttpRequest.set_timeout(t, xhr)
      }
      let () = Web.XMLHttpRequest.set_withCredentials(withCredentials, xhr)
    }
    let () = Web.XMLHttpRequest.send(body, xhr)
  })
}

@val external encodeURIComponent: string => string = "encodeURIComponent"

let encodeUri = str => encodeURIComponent(str)

@val external decodeURIComponent: string => string = "decodeURIComponent"

let decodeUri = str =>
  try Some(decodeURIComponent(str)) catch {
  | _ => None
  }

module Progress = {
  /*
  type bytesProgressed =
    { bytes : int
    ; bytesExpected : int
    }

  type ('data, 'parseFailData) t =
    | NoProgress
    (* SomeProgress (bytes, bytesExpected) *)
    | SomeProgress of bytesProgressed
    | FailProgress of 'parseFailData error
    | DoneProgress of 'data

  type ('msg, 'parseFailData) trackedRequest =
    { request : 'msg rawRequest
    ; toProgress : bytesProgressed -> 'msg
    ; toError : 'parseFailData error -> 'msg
    }
 */

  type t = {
    bytes: int,
    bytesExpected: int,
  }

  let emptyProgress = {
    bytes: 0,
    bytesExpected: 0,
  }

  /* Yeah this does not follow the original API, but that original
     API is... not extensible...  Instead, we have generic event
     listener support here so no need to constrain the API.
     Might still want to make a subscription variant though... */
  let track = (toMessage, Request(request, events)) => {
    let onprogress = Some(
      (callbacks, ev) => {
        open Vdom
        let lengthComputable = {
          open Tea_json.Decoder

          switch decodeValue(field("lengthComputable", bool), ev) {
          | Error(_e) => false
          | Ok(v) => v
          }
        }
        if lengthComputable {
          open Tea_json.Decoder

          let decoder = map2(
            (bytes, bytesExpected) => {bytes: bytes, bytesExpected: bytesExpected},
            field("loaded", int),
            field("total", int),
          )

          switch decodeValue(decoder, ev) {
          | Error(_e) => ()
          | Ok(t) => callbacks.contents.enqueue(toMessage(t))
          }
        }
      },
    )
    let events = switch events {
    | None => emptyRequestEvents
    | Some(e) => e
    }
    Request(request, Some({...events, onprogress: onprogress}))
  }
}
