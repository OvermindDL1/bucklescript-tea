let first: (result<'msg, 'b>, result<'c, 'b>) => result<'msg, 'b>

let result_to_option: result<'msg, 'b> => option<'msg>

let error_of_first: (result<'a, 'b>, result<'c, 'b>) => option<'b>
