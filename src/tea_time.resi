type t = float

let every: (~key: string, float, float => 'msg) => Tea_sub.t<'msg>

let delay: (float, 'msg) => Tea_cmd.t<'msg>

let millisecond: float

let second: float

let minute: float

let hour: float

let inMilliseconds: 'msg => 'msg

let inSeconds: float => float

let inMinutes: float => float

let inHours: float => float
