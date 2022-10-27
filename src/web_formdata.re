class type _formdata =
  {
    pub append: (string, string) => unit;
  };
  /* method append_blob : string -> Web_blob.t -> string -> unit */
type t = Js.t(_formdata);

[@bs.new] external create: unit => t = "FormData";

let append = (key, value, f) => f##append(key, value);
