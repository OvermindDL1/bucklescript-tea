let minInt: int

let maxInt: int

let minFloat: float

let maxFloat: float

type t<'typ>

let bool: t<bool>

let int: (int, int) => t<int>

let float: (float, float) => t<float>

let list: (int, t<'msg>) => t<list<'msg>>

let map: ('msg => 'b, t<'msg>) => t<'b>

let map2: (('msg, 'b) => 'c, t<'msg>, t<'b>) => t<'c>

let map3: (('msg, 'b, 'c) => 'd, t<'msg>, t<'b>, t<'c>) => t<'d>

let map4: (('msg, 'b, 'c, 'd) => 'e, t<'msg>, t<'b>, t<'c>, t<'d>) => t<'e>

let map5: (('msg, 'b, 'c, 'd, 'e) => 'f, t<'msg>, t<'b>, t<'c>, t<'d>, t<'e>) => t<'f>

let andThen: ('msg => t<'b>, t<'msg>) => t<'b>

let pair: (t<'msg>, t<'b>) => t<('msg, 'b)>

let generate: ('msg => 'b, t<'msg>) => Tea_cmd.t<'b>

type seed = Seed(Random.State.t)

let step: (t<'msg>, seed) => ('msg, seed)

let initialSeed: int => seed
