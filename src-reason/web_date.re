type t = Js.t({.});

type date_obj = {. [@bs.meth] "now": unit => float};

[@bs.new] external create_date : unit => t = "Date";

[@bs.val] external date_obj : date_obj = "Date";

let now = () => date_obj##now();
