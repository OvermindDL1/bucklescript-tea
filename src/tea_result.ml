let result_to_option = function
  | Ok a -> Some a
  | Error _ -> None

let first fst = function
  | Error _ as e -> e
  | Ok _ -> fst

let error_of_first fst = function
  | Error e -> Some e
  | Ok _ ->
    (match fst with
     | Ok _ -> None
     | Error e -> Some e)
