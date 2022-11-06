type never

type rec t<'succeed, 'fail> = Task((result<'succeed, 'fail> => unit) => unit): t<'succeed, 'fail>

let nothing: unit => unit

let performOpt: ('value => option<'msg>, t<'value, never>) => Tea_cmd.t<'msg>

let perform: ('value => 'msg, t<'value, never>) => Tea_cmd.t<'msg>

let attemptOpt: (Belt.Result.t<'succeed, 'fail> => option<'msg>, t<'succeed, 'fail>) => Tea_cmd.t<
  'msg,>

let attempt: (result<'succeed, 'fail> => 'msg, t<'succeed, 'fail>) => Tea_cmd.t<'msg>

let ignore: t<'msg, 'b> => Tea_cmd.t<'c>

let succeed: 'v => t<'v, 'e>

let fail: 'v => t<'e, 'v>

let nativeBinding: ((Belt.Result.t<'succeed, 'fail> => unit) => unit) => t<'succeed, 'fail>

let andThen: ('msg => t<'b, 'c>, t<'msg, 'c>) => t<'b, 'c>

let onError: ('msg => t<'b, 'c>, t<'b, 'msg>) => t<'b, 'c>

let fromResult: result<'success, 'failure> => t<'success, 'failure>

let mapError: ('msg => 'b, t<'c, 'msg>) => t<'c, 'b>

let toOption: t<'msg, 'b> => t<option<'msg>, 'c>

let map: ('msg => 'b, t<'msg, 'c>) => t<'b, 'c>

let map2: (('msg, 'b) => 'c, t<'msg, 'd>, t<'b, 'd>) => t<'c, 'd>

let map3: (('msg, 'b, 'c) => 'd, t<'msg, 'e>, t<'b, 'e>, t<'c, 'e>) => t<'d, 'e>

let map4: (('msg, 'b, 'c, 'd) => 'e, t<'msg, 'f>, t<'b, 'f>, t<'c, 'f>, t<'d, 'f>) => t<'e, 'f>

let map5: (('msg, 'b, 'c, 'd, 'e) => 'f, t<'msg, 'g>, t<'b, 'g>, t<'c, 'g>, t<'d, 'g>, t<'e, 'g>) => t<'f,'g,>

let map6: (
  ('msg, 'b, 'c, 'd, 'e, 'f) => 'g,
  t<'msg, 'h>,
  t<'b, 'h>,
  t<'c, 'h>,
  t<'d, 'h>,
  t<'e, 'h>,
  t<'f, 'h>,
) => t<'g, 'h>

let sequence: list<t<'msg, 'b>> => t<list<'msg>, 'b>

let testing_deop: ref<bool>

let testing: unit => unit
