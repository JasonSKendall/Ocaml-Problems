let rec print_list_int myList  = match myList with
    | [] -> 
        print_endline "";
    | head::body -> 
        print_int head; 
        print_string " ";
        print_list_int body 
    ;;

let print_current_pair i j k =
    let thing = Printf.sprintf "-> %i %i %i\n%!" i j k in
    print_string thing 
    ;;

let two_sum mylist1 target =
    print_list_int mylist1 ;
    let mylist2 = List.rev mylist1 in
    print_list_int mylist2 ;
    let rec find_pair mylist1 mylist2 target =
        match mylist1, mylist2 with
        | [] , _ | _ , [] -> []
        | hd1 :: tl1 , hd2 :: tl2 ->
            let diff = hd2 + hd1 - target in
            print_current_pair hd1 hd2 diff;
            if ( diff == 0 ) then
                [ hd1 ; hd2]
            else
                    if ( diff > 0 ) then
                        find_pair mylist1 tl2 target
                    else
                        find_pair tl1 mylist2 target
    in
    let is_it = find_pair mylist1 mylist2 target in
    let is_emptee = List.compare_length_with is_it 0 != 0 in
    print_list_int is_it ;
    let thing = Printf.sprintf "Is this good for two_sum? %B\n%!" is_emptee in
    print_string thing 


let a = [-2; 1; 2; 4; 6; 7; 18; 19 ] ;;
let target = 1 ;; 


two_sum a target ;;






