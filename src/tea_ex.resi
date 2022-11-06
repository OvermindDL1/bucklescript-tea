let render_event: (~key: string=?, 'msg) => Tea_sub.t<'msg>

module LocalStorage: {
  let length: Tea_task.t<int, string>
  let clear: Tea_task.t<unit, string>
  let clearCmd: unit => Tea_cmd.t<'msg>
  let key: int => Tea_task.t<option<string>, string>
  let getItem: string => Tea_task.t<option<string>, string>
  let removeItem: string => Tea_task.t<unit, string>
  let removeItemCmd: string => Tea_cmd.t<'msg>
  let setItem: (string, string) => Tea_task.t<unit, string>
  let setItemCmd: (string, string) => Tea_cmd.t<'msg>
}
