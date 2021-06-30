type 'a pointer = Null | Pointer of 'a ref;;

let ( !^ ) = function
    | Null -> invalid_arg "Attempt to dereference the null pointer"
    | Pointer r -> !r;;

let ( ^:= ) p v =
    match p with
    | Null -> invalid_arg "Attempt to assign the null pointer"
    | Pointer r -> r := v;;

let new_pointer x = Pointer (ref x);;