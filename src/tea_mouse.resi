type position = {x: int, y: int}
let position: Tea_json.Decoder.t<Js.Json.t, position>
let registerGlobal: (string, string, position => 'a) => Tea_sub.t<'a>
let clicks: (~key: string=?, position => 'a) => Tea_sub.t<'a>
let moves: (~key: string=?, position => 'a) => Tea_sub.t<'a>
let downs: (~key: string=?, position => 'a) => Tea_sub.t<'a>
let ups: (~key: string=?, position => 'a) => Tea_sub.t<'a>
