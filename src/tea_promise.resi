let cmd: (Js.Promise.t<'msg>, 'msg => option<'b>) => Tea_cmd.t<'b>

let result: (Js.Promise.t<'msg>, result<'msg, string> => 'b) => Tea_cmd.t<'b>
