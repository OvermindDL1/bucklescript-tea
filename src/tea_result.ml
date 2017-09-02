

(* TODO:  Remove this when Bucklescript is updated to OCaml 4.03 as it includes result *)
type ('a, 'b) t (* result *) =
  | Ok of 'a
  | Error of 'b

let result_to_option = function
  | Ok a -> Some a
  | Error _ -> None
