
let make_list_up_to_sqrt n =
    let this_max = Int.of_float ( sqrt ( Float.of_int n ) ) in
    Array.init this_max (fun i -> i + 2 ) |> Array.to_list
in

let rec prime_checker l n =
    match l with
    | [] -> true
    | hd :: tl ->
        if ( Int.rem n hd == 0 ) then
            false
        else
            prime_checker ( List.filter ( fun i -> Int.rem i hd != 0) tl ) n
in

let am_i_a_prime_number_prime n =
    let is_it = prime_checker (make_list_up_to_sqrt n) n in
    let thing = Printf.sprintf "Is %i prime? %B\n%!" n is_it in
    print_string thing 

in

    
(*
am_i_a_prime_number_prime 2;
am_i_a_prime_number_prime 3;
am_i_a_prime_number_prime 5;
am_i_a_prime_number_prime 50;
am_i_a_prime_number_prime 934223;
am_i_a_prime_number_prime 47396711;
am_i_a_prime_number_prime 473396711;
am_i_a_prime_number_prime 4739696711;
*)
am_i_a_prime_number_prime 47239696787;


