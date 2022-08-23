type response_status = {code: int, message: string}
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
let string_of_error: error<'a> => string
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
let emptyRequestEvents: requestEvents<'a>
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
let expectStringResponse: (string => result<'a, string>) => expect<'a>
let expectString: expect<string>
let request: rawRequest<'a> => request<'b, 'a>
let getString: string => request<'a, string>
let toTask: request<'a, 'b> => Tea_task.t<'b, error<string>>
let send: (result<'a, error<string>> => 'b, request<'b, 'a>) => Tea_cmd.t<'b>
@val external encodeURIComponent: string => string = "encodeURIComponent"
let encodeUri: string => string
@val external decodeURIComponent: string => string = "decodeURIComponent"
let decodeUri: string => option<string>
module Progress: {
  type t = {bytes: int, bytesExpected: int}
  let emptyProgress: t
  let track: (t => 'a, request<'a, 'b>) => request<'a, 'b>
}
