
type _formdata =()

@send external append: ((),string, string) => unit="append"

type t = _formdata

@new external create: unit => t = "FormData"

let append = (key, value, f) => append(f,key, value)