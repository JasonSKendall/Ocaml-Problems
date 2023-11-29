
(* This checker takes the number, the max value of the Seive, and list
of all numbers up to the SQRT of the needed number.  We also have to filter
out the un-needed numbers, since we're not going to create the full seive
before doing the test.  *)

let rec prime_checker n m l =
    match l with
    | [] -> true
    | hd :: tl ->
        if ( Int.rem n hd == 0 ) then
            false
        else
            prime_checker n m (
            if ( hd < m ) then
                List.filter ( fun i -> Int.rem i hd != 0) tl 
            else
                tl 
            )
in


let get_int_of_sqrt i =
    Int.of_float ( sqrt ( Float.of_int i ) )
in

let make_list_up_to_sqrt n =
    Array.init 
    ( get_int_of_sqrt n)
    ( fun i -> i + 2 ) |> Array.to_list
in

let am_i_a_prime_number_prime n =
    let max_of_seive = get_int_of_sqrt (get_int_of_sqrt ( n + 1 )) in
    let is_it = prime_checker n max_of_seive (make_list_up_to_sqrt n) in
    let thing = Printf.sprintf "Is %i prime? %B\n%!" n is_it in
    print_string thing 

in

    
am_i_a_prime_number_prime 2;
am_i_a_prime_number_prime 3;
am_i_a_prime_number_prime 5;
am_i_a_prime_number_prime 50;
am_i_a_prime_number_prime 934223;
am_i_a_prime_number_prime 47396711;
am_i_a_prime_number_prime 473396711;
am_i_a_prime_number_prime 4739696711;
am_i_a_prime_number_prime 47239696787;
am_i_a_prime_number_prime 472309696787;

