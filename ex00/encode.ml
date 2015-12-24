let encode_list l = l

let encode l = match l with
    | [] -> []
    | _ -> encode_list l
