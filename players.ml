#use "sig_player.ml" ;;
#use "game.ml" ;;

module TestHumanPlayer =
struct
  module PlayerGame = ConnectFour
  open PlayerGame

(* next_move *)
    (* given a state,selects a move, if the selected move is not legal, the user
       will be prompted again *)
    (* Input: the state of the game, s
       Output: a legal move *)
    let rec next_move s =
      try
        let m = move_of_string (read_line ()) in
        if List.mem m (legal_moves s) then m
        else
          let () = print_endline "Illegal move."
          in next_move s
      with
      | End_of_file -> failwith "exiting."
      | Failure message -> print_endline message ; next_move s
end ;;

(* Because next_move involves read input, you don't need to test it here.
 * If you use any helper procedures in TestHumanPlayer, test them here. *)

module HumanPlayer =
  (TestHumanPlayer : PLAYER with module PlayerGame := ConnectFour) ;;

module TestAIPlayer =
struct
  module PlayerGame = ConnectFour
  open PlayerGame

(* next_move *)
    (* given a state, selects a move, based on optimizing expected value *)
    (* Input: the state of the game, s
       Output: a legal move *)
(* NESTED HELPERS *)
    (* all_next_states gets all future states of the game given a list of moves
       and the current state *)
    (* pick_middle_move selects the middle column if no others are filled in *)
    (* winning_move returns a move that would end the game, if any exist *)
    (* block_winning_move returns a move that would block the opponent from
       winning the game on their next move, if any exist *)
    (* minimax looks ahead a certain number of moves and selects the move that
       would be most beneficial to the current player assuming their opponent
       always used the same logic *)
    (* get_max_minimax returns the value and move associated with the maximum
       value of all input moves *)
    (* get_min_minimax returns the value and move associated with the minimum
       value of all input moves *)
    (* pass_to_minimax continues the descending the tree of moves using the
       minimax algorithm *)
(* NOTE *)
    (* We chose to check for potential winning moves and potential ways to block
       the opponent from winning on their next turn before calling the minimax
       algorithm to account for the fact that the opponent does not use the same
       logic and might not play optimally. This makes a difference when the AI
       realizes that their may be a guarenteed path for one player to win within
       the a given number of moves. This leads to different moves having the
       same value, which comes into play when one move might let the opponent
       win on their next move, and another moves doesn't let them win until 3
       more moves. This relies on the assumption that the opponent will play
       optimally, but that assumption often does not hold and it is therefore
       advantageous for the AI to look forward only one move before looking
       forward many moves to make his decision. Additionally, looking forward
       1-2 moves does not impact the runtime, since the minimax algorithm runs
       in n^5 time and these extra lookups can be dropped when using big O,
       additionally, this speeds up game play when there is an obvious made that
       needs to be made*)
    let next_move s =
      let initial_moves = legal_moves initial_state in
      let middle_move = (List.nth initial_moves
                           ((List.length initial_moves)/2)) in

      let rec pick_middle_move (move : move) (curr_state : state)
          (mid_state : state) : move option =
        if List.mem move (legal_moves mid_state) then
          if curr_state = mid_state then (Some move) else
            pick_middle_move move curr_state (next_state mid_state move)
        else None in

      let mid_move = pick_middle_move middle_move s initial_state in
      if (List.length initial_moves) mod 2 = 1 && not (mid_move = None) then
        match mid_move with
        | Some move -> move
        | None -> failwith "something wrong with matching"
      else

      let rec all_next_states (moves : move list) (state : state) : state list =
        match moves with
        | [] -> []
        | hd::tl -> (next_state state hd)::(all_next_states tl state) in

      let rec winning_move (moves : move list) (states : state list)
          (orig_moves : move list): move option =
          match moves,states with
          | [],[] -> None
          | hd1::tl1,hd2::tl2 -> let win = game_status hd2 in
            if (win = Win P1 || win = Win P2) &&
               not (List.mem hd1 orig_moves) then Some hd1
                                  else winning_move tl1 tl2 orig_moves
          | _,_ -> failwith "different length inputs" in

      let rec block_winning_move (states : state list) (moves : move list)
        : move option =
          match states,moves with
          | [],[] -> None
          | hd1::tl1,hd2::tl2 -> (let leg_moves = legal_moves hd1 in
                                  let new_states = all_next_states leg_moves
                                      hd1 in
                                  let losing_move = winning_move leg_moves
                                      new_states [hd2] in
                                match losing_move with
                                | None -> block_winning_move tl1 tl2
                                | _ -> losing_move)
          | _,_ -> failwith "diff input lengths" in

      let rec minimax (state : state) (depth : int) (move : move option)
        : float * (move option) =
        let rec get_max_minimax (lst : (float * (move option)) list)
            (max_val : float * (move option)) (moves : move list)
          : float * (move option) =
              match lst,max_val,moves with
              | [],_,_ -> max_val
              | (curr_val,curr_move)::tl,(curr_max,_),hd::tl2 -> if
                curr_val = max_float then (curr_val,curr_move) else
                if curr_val > curr_max then get_max_minimax tl
                    (curr_val,Some hd) tl2
              else get_max_minimax tl max_val tl2
              | _,_,_ -> failwith "different input lengths" in
        let rec get_min_minimax (lst : (float * (move option)) list)
            (min_val : float * (move option)) (moves : move list)
          : float * (move option) =
              match lst,min_val,moves with
              | [],_,_ -> min_val
              | (curr_val,curr_move)::tl,(curr_min,_),hd::tl2 ->
                if curr_val = (-1. *. max_float) then (curr_val,curr_move) else
                if curr_val < curr_min then get_min_minimax tl
                    (curr_val,Some hd) tl2
              else get_min_minimax tl min_val tl2
              | _,_,_ -> failwith "different input lengths" in
          if depth > 5 then (estimate_value state, move) else
          let status = game_status state in
          let moves = legal_moves state in
          let new_states = all_next_states moves state in
              match status with
              | Win P1 -> (max_float, move)
              | Win P2 -> ((-1. *. max_float), move)
              | Draw -> (0.0, move)
              | Ongoing P1 -> get_max_minimax (pass_to_minimax new_states
                                                 depth moves)
                                ((-1. *. max_float),move) moves
              | Ongoing P2 -> get_min_minimax (pass_to_minimax new_states
                                                 depth moves)
                                (max_float,move) moves

          and

            pass_to_minimax (states : state list) (depth : int)
              (moves : move list) : (float * (move option)) list =
                  match states,moves with
                  | [],[] -> []
                  | hd1::tl1,hd2::tl2 -> (minimax hd1 (depth+1)
                                            (Some hd2))::(pass_to_minimax
                                                            tl1 depth tl2)
                  | _,_ -> failwith "different lengths" in

        let moves = legal_moves s in
        let new_states = all_next_states moves s in
        let winner = winning_move moves new_states [] in
            match winner with
            | Some move -> move
            | None ->
                let blocked = block_winning_move new_states moves in
                    match blocked with
                    | Some move -> move
                    | None ->
                        let num_moves = List.length moves in
                        let random_move = if num_moves = 0 then None
                        else Some (List.nth moves (Random.int num_moves)) in
                        let (value,move) = minimax s 1 random_move in
                            match move with
                            | None -> failwith "no moves found"
                            | Some move -> let status = game_status s in
                              match status with
                              | Ongoing P1 -> if value = max_float then
                                  (print_string (("\027[34m" ^ "\027[5m" ^
                                                "\027[1m" ^
                                                "GET READY TO LOSE!!!" ^
                                                  "\027[0m" ^ "\n")) ; move)
                                else move
                              | Ongoing P2 -> if value = (-1. *. max_float) then
                                  (print_string (("\027[34m" ^ "\027[5m" ^
                                                "\027[1m" ^
                                                "GET READY TO LOSE!!!" ^
                                                  "\027[0m" ^ "\n")) ; move)
                                else move
                              | _ -> failwith "not a valid state"

end ;;

(* NOTE *)
(* Since all helpers are nested within next_move, they cannot be tested *)

module AIPlayer =
  (TestAIPlayer : PLAYER with module PlayerGame := ConnectFour) ;;
