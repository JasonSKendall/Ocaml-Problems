

let rec two_sum mylist target =
    match mylist with
        | [] -> []
        | hd :: tl ->
            (*
            print_string "-> ";
            print_int hd;
            print_endline "";
            *)
            let diff = target - hd in
            match List.exists ( (=) diff ) tl  with
                | true -> [ hd ; diff ]
                | false -> two_sum tl target
    ;;

let rec print_list_int myList  = match myList with
    | [] -> 
        print_endline "";
    | head::body -> 
        print_int head; 
        print_string " ";
        print_list_int body 
    ;;


let a = [-2; 1; 2; 4; 7; 11; 19 ] ;;
let target = 13 ;; 

let z = List.rev a ;;

let () = 
    print_list_int a  ;
    let is_it = two_sum a target in
    print_list_int is_it ;
    let is_emptee = List.compare_length_with is_it 0 != 0 in
    let thing = Printf.sprintf "Is this good for two_sum? %B\n%!" is_emptee in
    print_string thing 
    ;;

print_list_int z ;;






