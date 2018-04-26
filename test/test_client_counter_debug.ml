open Tea.App
open Tea.Html

type msg =
  | Increment
  | Decrement
  | Reset
  | Set of int

let update model = function
  | Increment -> model + 1, Tea.Cmd.none
  | Decrement -> model - 1, Tea.Cmd.none
  | Reset -> 0, Tea.Cmd.none
  | Set v -> v, Tea.Cmd.none


let view_button title msg =
  button
    [ onClick msg
    ]
    [ text title
    ]

let view model =
  div
    []
    [ span
        [ style "text-weight" "bold" ]
        [ text (string_of_int model) ]
    ; br []
    ; view_button "Increment" (
        if model >= 3 then
          Decrement
        else
          Increment
      )
    ; br []
    ; view_button "Decrement" Decrement
    ; br []
    ; view_button "Set to 42" (Set 42)
    ; br []
    ; if model <> 0 then view_button "Reset" Reset else noNode
    ]

module D = Tea.Debug.MakeStandardProgram(struct
  type cflags = unit
  type cmsg = msg
  type cmodel = int
  let init = fun () -> 4, Tea.Cmd.none
  let update = update
  let view = view
  let subscriptions = fun _ -> Tea.Sub.none
  let string_of_msg = function
    | Increment -> "Increment"
    | Decrement -> "Decrement"
    | Reset -> "Reset"
    | Set _ -> "Set"
end)

let main = D.start
