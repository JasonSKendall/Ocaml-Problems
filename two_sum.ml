let rec print_list_int myList  = match myList with
    | [] -> 
        print_endline "";
    | head::body -> 
        print_int head; 
        print_string " ";
        print_list_int body 
    ;;

let print_current_pair_and_target i j k =
    let thing = Printf.sprintf "-> %i %i %i\n%!" i j k in
    print_string thing 
    ;;

let print_current_pair i j =
    let thing = Printf.sprintf "-> %i %i\n%!" i j in
    print_string thing 
    ;;


let rec two_sum_brute_force mylist target =
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
                | false -> two_sum_brute_force tl target
    ;;






let two_sum_rev_list mylist1 target =
    let mylist2 = List.rev mylist1 in
    (* print_list_int mylist2 ; *)
    let rec find_pair mylist1 mylist2 target =
        match mylist1, mylist2 with
        | [] , _ | _ , [] -> []
        | hd1 :: tl1 , hd2 :: tl2 ->
            let diff = hd2 + hd1 - target in
            (* print_current_pair_and_target hd1 hd2 diff; *)
            if ( diff == 0 ) then
                [ hd1 ; hd2]
            else
                if ( diff > 0 ) then
                    find_pair mylist1 tl2 target
                else
                    find_pair tl1 mylist2 target
    in
    find_pair mylist1 mylist2 target 
    ;;









let two_sum_make_compare mylist target =
    let keeper_list = [] in
    let rec find_pair mylist target keeper_list =
        match mylist with
        | [] -> []
        | hd :: tl ->
            match List.exists ( (=) ( target - hd ) ) keeper_list with
            | true -> [ (target - hd) ; hd ]
            | false -> find_pair tl target ( hd :: keeper_list )
    in
    find_pair mylist target keeper_list
    ;;





let a = [-2; -1 ; 1; 2; 4; 6; 8; 11; 19 ] ;;
let target = 13 ;; 

print_list_int a ;;

let () = 
    let is_it = two_sum_brute_force a target in
    (* print_list_int is_it ; *)
    let is_emptee = List.compare_length_with is_it 0 != 0 in
    let thing = Printf.sprintf "Brute force:\n Is this good for two_sum? %B\n%!" is_emptee in
    print_string thing 
    ;;

let () =
    let is_it = two_sum_rev_list a target in
    let is_emptee = List.compare_length_with is_it 0 != 0 in
    (* print_list_int is_it ; *)
    let thing = Printf.sprintf "Reverse List:\n Is this good for two_sum? %B\n%!" is_emptee in
    print_string thing 
    ;;

let () =
    let is_it = two_sum_make_compare a target in
    let is_emptee = List.compare_length_with is_it 0 != 0 in
    (* print_list_int is_it ; *)
    let thing = Printf.sprintf "Create Comparision list:\n Is this good for two_sum? %B\n%!" is_emptee in
    print_string thing 
    ;;


