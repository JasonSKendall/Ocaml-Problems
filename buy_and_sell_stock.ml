let rec print_list_int myList  = match myList with
    | [] -> 
        print_endline "";
    | head::body -> 
        print_int head; 
        print_string " ";
        print_list_int body 
    ;;


let rec buy_and_sell_stock_once prices maxdiff current_min =
    match prices with
    | [] -> maxdiff
    | hd :: tl ->
        (*
        print_int hd;
        print_string " ";
        print_int current_min;
        print_string " ";
        print_int maxdiff;
        print_endline "";
           *)

        let test_maxdiff = hd - current_min in
        if ( hd < current_min ) then
            buy_and_sell_stock_once tl maxdiff hd
        else if (maxdiff < test_maxdiff ) then
            buy_and_sell_stock_once tl test_maxdiff current_min
            else
                buy_and_sell_stock_once tl maxdiff current_min
  
    ;;




let get_output a =
    print_list_int a ;
    let is_it = buy_and_sell_stock_once a 0 1000000000000 in
    let thing = Printf.sprintf "Max Profit: %i\n\n%!" is_it in
    print_string thing 
    ;;


get_output [310; 315; 275; 295; 260; 270; 290; 230; 255; 250] ;

get_output [-2; -1 ; 1; 2; 4; 6; 8; 11; 19 ] ;

get_output [100; 180; 260; 310; 40; 535; 695] ;

get_output [50; 40; 30; 20; 10] ;

get_output [110; 215; 180; 335; 5] ;

