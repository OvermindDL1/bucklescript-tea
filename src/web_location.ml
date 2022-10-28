
type t =
  { href : string
  ; protocol : string
  ; host : string
  ; hostname : string
  ; port : string
  ; pathname : string
  ; search : string
  ; hash : string
  ; username : string
  ; password : string
  ; origin : string
  }

let get () : t =
  let open Webapi.Dom in
  let open Location in
  { href = href(location)
  ; protocol = protocol(location)
  ; host = host(location)
  ; hostname = hostname(location)
  ; port = port(location)
  ; pathname = pathname(location)
  ; search = search(location)
  ; hash = hash(location)
  ; username = username(location)
  ; password = password(location)
  ; origin = origin(location)
  }