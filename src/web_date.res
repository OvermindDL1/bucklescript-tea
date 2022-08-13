type t = {.}

type date_obj 
@send external now: date_obj => float = "now"

@new external create_date: unit => t = "Date"

@val external date_obj: date_obj = "Date"

let now =  () => now (date_obj)
