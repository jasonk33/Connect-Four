#use "players.ml" ;;

module Referee =
struct
  (* Change these module names to what you've named them *)
  module CurrentGame = ConnectFour
  module Human : PLAYER with module PlayerGame := CurrentGame = HumanPlayer
  module AI    : PLAYER with module PlayerGame := CurrentGame = AIPlayer

  open CurrentGame

  (* prompts a user to select a game option.
   * I/P: a unit
   * O/P: None, if the user quits
   *      Some (i), where i is the index of
   *                the gametype they select *)
  let rec poll_menu () : int =
    print_endline "1. Human v Human" ;
    print_endline "2. Human v    AI" ;
    print_endline "3. AI    v Human" ;
    print_endline "4. AI    v    AI" ;
    print_endline "Please select a game type [1-4] or (q)uit." ;
    try
      match read_line () with
      | "quit" | "q" | "exit" -> raise End_of_file
      | input -> let number = int_of_string input in
        if number >= 1 && number <= 4 then number else
        let () = print_endline ("Please enter a " ^
        											  "number between 1 and 4.")
        in poll_menu ()
    with
    | End_of_file -> failwith "exiting."
    | _ -> print_endline "Invalid input." ; poll_menu ()

  let play_game () : unit =
    let p1_next_move, p2_next_move =
      match poll_menu () with
        | 1 -> Human.next_move, Human.next_move
        | 2 -> Human.next_move,    AI.next_move
        | 3 ->    AI.next_move, Human.next_move
        | 4 ->    AI.next_move,    AI.next_move
        | _ ->  failwith "EMF" in
    let rec game_loop (s : state) : unit =
      print_endline (string_of_state s) ;
      match game_status s with
      | Win player -> print_endline ("\027[34m" ^ "\027[5m" ^ "\027[1m" ^
                                     (string_of_player player) ^ " wins!" ^
                                     "\027[0m")
      | Draw -> print_endline ("\027[34m" ^ "\027[5m" ^ "\027[1m" ^ "Draw..."
                               ^ "\027[0m")



      | Ongoing player ->
        print_endline ((string_of_player player) ^ "'s turn.") ;
        let move = (match player with
                    | P1 -> p1_next_move s
                    | P2 -> p2_next_move s) in
        print_endline ((string_of_player player) ^
                        " makes the move " ^
                        (string_of_move move)) ;
        game_loop (next_state s move)
    in try game_loop initial_state with
       | Failure message -> print_endline message

end ;;

Referee.play_game () ;;
