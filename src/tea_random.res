let () = Random.self_init()

let minInt = min_int

let maxInt = max_int

let minFloat = min_float

let maxFloat = max_float

type t<'typ> = Generator(Random.State.t => 'typ)

let bool = Generator(state => Random.State.bool(state))

let int = (min, max) => {
  let (min, max) = if min < max {
    (min, max)
  } else {
    (max, min)
  }
  Generator(state => min + Random.State.int(state, max - min + 1))
}

let float = (min, max) => Generator(state => min +. Random.State.float(state, max -. min))

let list = (count, Generator(genCmd)) => {
  let rec buildList = (state, i, acc) =>
    if i > 0 {
      buildList(state, i - 1, list{genCmd(state), ...acc})
    } else {
      acc
    }
  Generator(state => buildList(state, count, list{}))
}

let map = (func, Generator(genCmd)) => Generator(state => func(genCmd(state)))

let map2 = (func, Generator(genCmd1), Generator(genCmd2)) => Generator(
  state => {
    let res1 = genCmd1(state)
    let res2 = genCmd2(state)
    func(res1, res2)
  },
)

let map3 = (func, Generator(genCmd1), Generator(genCmd2), Generator(genCmd3)) => Generator(
  state => {
    let res1 = genCmd1(state)
    let res2 = genCmd2(state)
    let res3 = genCmd3(state)
    func(res1, res2, res3)
  },
)

let map4 = (
  func,
  Generator(genCmd1),
  Generator(genCmd2),
  Generator(genCmd3),
  Generator(genCmd4),
) => Generator(
  state => {
    let res1 = genCmd1(state)
    let res2 = genCmd2(state)
    let res3 = genCmd3(state)
    let res4 = genCmd4(state)
    func(res1, res2, res3, res4)
  },
)

let map5 = (
  func,
  Generator(genCmd1),
  Generator(genCmd2),
  Generator(genCmd3),
  Generator(genCmd4),
  Generator(genCmd5),
) => Generator(
  state => {
    let res1 = genCmd1(state)
    let res2 = genCmd2(state)
    let res3 = genCmd3(state)
    let res4 = genCmd4(state)
    let res5 = genCmd5(state)
    func(res1, res2, res3, res4, res5)
  },
)

let andThen = (func, Generator(genCmd)) => Generator(
  state => {
    let res = genCmd(state)
    let Generator(userGen) = func(res)
    userGen(state)
  },
)

let pair = (gen1, gen2) => map2((a, b) => (a, b), gen1, gen2)

let generate = (tagger, Generator(genCmd)) =>
  Tea_cmd.call(callbacks => {
    let state = Random.get_state()
    let genValue = genCmd(state)
    let () = Random.set_state(state)
    open Vdom
    callbacks.contents.enqueue(tagger(genValue))
  })

/* Generate Values Manually */

type seed = Seed(Random.State.t)

let step = (Generator(genCmd), Seed(state)) => {
  let newState = Random.State.copy(state)
  (genCmd(newState), Seed(newState))
}

let initialSeed = seed => Seed(Random.State.make([seed]))
