type t = {time: Tea_time.t, delta: Tea_time.t}

let every: (~key: string=?, t => 'msg) => Tea_sub.t<'msg>

let times: (~key: string=?, (~key: string, Tea_time.t) => 'msg) => Tea_sub.t<'msg>

let diffs: (~key: string=?, (~key: string, Tea_time.t) => 'msg) => Tea_sub.t<'msg>
