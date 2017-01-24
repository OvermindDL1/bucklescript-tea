

(* TODO:  Remove this when Bucklescript is updated to OCaml 4.03 as it includes result *)
type ('a, 'b) t (* result *) =
  | Ok of 'a
  | Error of 'b
