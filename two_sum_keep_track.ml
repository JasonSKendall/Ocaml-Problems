let rec print_list_int myList  = match myList with
    | [] -> 
        print_endline "";
    | head::body -> 
        print_int head; 
        print_string " ";
        print_list_int body 
    ;;

let print_current_pair i j =
    let thing = Printf.sprintf "-> %i %i\n%!" i j in
    print_string thing 
    ;;

let two_sum mylist target =
    let keeper_list = [] in
    let rec find_pair mylist target keeper_list =
        match mylist with
        | [] -> false
        | hd :: tl ->
            match List.exists ( (=) ( target - hd ) ) keeper_list with
            | true -> true
            | false -> find_pair tl target ( hd :: keeper_list )
    in
    find_pair mylist target keeper_list
    ;;

let a = [-2; 1; 2; 4; 6; 8; 11; 19 ] ;;
let target = 13 ;; 

let () =
    print_list_int a ;
    let is_it = two_sum a target in
    let thing = Printf.sprintf "Is this good for two_sum? %B\n%!" is_it in
    print_string thing 
    ;;






