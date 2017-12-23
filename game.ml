#use "sig_game.ml" ;;
#use "CS17setup.ml" ;;

module TestGame =
struct
    (* TYPES *)

    (* this specifies a player:
     * which player won, which player's turn it is, etc. *)
    type which_player = P1 | P2

    (* represents the status of the game:
     * if it's over, who won,
     * who's turn it is, if it isn't *)
    type status =
    | Win of which_player
    | Draw
    | Ongoing of which_player

    (* represents the status of each coordinate in the board *)
    type hole = B | R | O

    (* expresses the state of the game,
    * i.e., what the board looks like, whose turn it is *)
    type state = hole list list * which_player

    (* describes a game's moves that a player can make *)
    type move = int

    (* represents the position of a spot in the board *)
    type coordinate = int * int

    (* represents all the ways you can get four in a row *)
    type row_type = Row | Column | LeftDiagonal | RightDiagonal

    (* mutable number of rows for the board *)
    let initial_rows = 5

    (* mutable number of columns for the board *)
    let initial_columns = 7

    (* PRINTING FUNCTIONS *)

(* string_of_player *)
    (* converts the internal representation of a player to a string *)
    (* Input: a player, player
       Output: the string representation of that player *)
    let string_of_player (player : which_player) : string =
        match player with
        | P1 -> "Player 1"
        | P2 -> "Player 2"

(* string_of_state *)
    (* converts the internal representation of a state to a string *)
    (* Input: the state of the game, (board,player)
       Output: the string representation of that state *)
(* NESTED HELPERS *)
    (* print_header prints a proper heading for the number of rows for the
      game *)
    (* board_to_string prints the string representation of the board *)
    (* row_to_string prints each row *)
    (* string_of_hole converts a hole to a string *)
    let string_of_state  (board,player : state) : string =
        let rec print_header (num : int) : string =
            if num > initial_columns then failwith "bad input"
            else if num = initial_columns then " (" ^(string_of_int num) ^ ")\n"
            else " (" ^ (string_of_int num) ^ ") " ^ (print_header (num+1)) in

        let rec board_to_string (board : hole list list) : string =
            let rec row_to_string (row : hole list) : string =
                let string_of_hole (hole : hole) : string =
                    match hole with
                    | B -> "\027[36m" ^ "\027[1m" ^ "B" ^ "\027[0m"
                    | R -> "\027[31m" ^ "\027[1m" ^ "R" ^ "\027[0m"
                    | O -> "\027[32m" ^ "\027[2m" ^ "O" ^ "\027[0m" in

                match row with
                | hd::[] -> "\027[35m" ^ "| " ^ "\027[0m" ^ (string_of_hole hd)
                            ^ "\027[35m" ^ " |" ^ "\027[0m"
                | hd::tl -> "\027[35m" ^ "| " ^ "\027[0m" ^ (string_of_hole hd)
                            ^ "\027[35m" ^ " |" ^ "\027[0m" ^ (row_to_string tl)
                | _ -> "" in

            match board with
            | hd::[] -> (row_to_string hd)
            | hd::tl -> (board_to_string tl) ^ "\n" ^ (row_to_string hd)
            | _ -> "" in

        "\027[33m" ^ (print_header 1) ^ "\027[0m" ^ (board_to_string board)

(* string_of_move *)
    (* converts the internal representation of a move to a string *)
    (* Input: a move, move
       Output: the string representation of that move *)
    let string_of_move  (move : move) : string =
      ":" ^ "\027[34m" ^ "\027[1m" ^ " Column " ^ (string_of_int (move+1))
      ^ "\027[0m"

    (* GAME LOGIC *)

(* initialize_list *)
    (* turns a datum into a list of that datum repeated *)
    (* Input: the number of times to repeat the datum in the list, length
              the datum to be repeated, datum
       Output: a repeated list of input length of the datum *)
    let rec initialize_list (length : int) (datum : 'a) : 'a list =
        if length = 0 then [] else [datum]@(initialize_list (length-1) datum)

    (* the state of the game when it begins *)
    let initial_state = (initialize_list initial_rows
        (initialize_list initial_columns O), P1)

(* transpose *)
    (* transposes the board to make each row into a column and vice versa *)
    (* Input: a board, board
       Output: a board, but transposed to switch the rows and columns *)
    let rec transpose (board: hole list list) : hole list list =
        match board with
        | (hd1 :: []) :: tl -> [(List.map List.hd ((hd1 :: []) :: tl))]
        | (hd1 :: tl1) :: tl -> [(List.map List.hd ((hd1 :: tl1) :: tl))]@(
            transpose (List.map List.tl ((hd1 :: tl1) :: tl)))
        | _ -> []

(* check_rows *)
    (* gets information for a single row (row, column, or diagonal) *)
    (* Input: the board, board
              the type of row, row_type
              the starting coordinates of the row, coordinate
              the number of pieces in a row to look for, num
       Output: a list of matches for the row given the number of pieces in a row
              to look for, containing the status, whether the previous/next hole
              is empty, and the coordinates of the previous/next hole *)
(* NESTED HELPERS *)
    (* num_in_row returns the matching color, whether the previous/next hole
       is empty, and the coordinates of the previous/next hole for a specified
       row and number of pieces to look for *)
    (* increment_row_type increments the coordinates for the next iteration
       given the row type *)
    let rec check_rows (board : hole list list) (row_type : row_type)
        (row_num,col_num : coordinate) (num : int) :
      (status * bool * bool * coordinate * coordinate) list =
      let rec num_in_row (row : hole list) (in_a_row : int) (color : hole)
          (num : int)
          (row_type : row_type) (row_num,col_num : coordinate)
          (previous_empty : bool) (previous_coord : coordinate) :
        hole * bool * bool * coordinate * coordinate =
        let increment_row_type (row_type : row_type)
            (row_num,col_num : coordinate) : coordinate =
                match row_type with
                | Row -> (row_num,col_num+1)
                | Column -> (row_num+1,col_num)
                | LeftDiagonal -> (row_num+1,col_num-1)
                | RightDiagonal -> (row_num+1,col_num+1) in

        if in_a_row = num then (color, (match row with O::_ -> true | _ ->
            false), previous_empty, (row_num,col_num), previous_coord) else
            let new_coord = (increment_row_type row_type (row_num,col_num)) in

                match row,color with
                | [],_ -> (O,false,false,(-1,-1),(-1,-1))
                | O::[],_ -> num_in_row [] 0 O num row_type new_coord true
                               (row_num,col_num)
                | O::O::tl,_ -> num_in_row (O::tl) 0 O num row_type new_coord
                                  true (row_num,col_num)
                | O::B::[],B -> if in_a_row = 2 && num = 3 then
                    (B, true, false, (row_num,col_num), (-1,-1))
                  else num_in_row [B] 0 O num row_type new_coord true
                      (row_num,col_num)
                | O::B::B::tl,B -> if in_a_row = 1 && num = 3 then
                    (B, true, false, (row_num,col_num), (-1,-1))
                  else num_in_row (B::B::tl) 0 O num row_type new_coord true
                      (row_num,col_num)
                | O::B::tl,B -> if in_a_row = 2 && num = 3 then
                    (B, true, false, (row_num,col_num), (-1,-1))
                  else num_in_row (B::tl) 0 O num row_type new_coord true
                      (row_num,col_num)
                | O::B::tl,_ -> num_in_row (B::tl) 0 O num row_type new_coord
                                  true (row_num,col_num)
                | O::R::[],R -> if in_a_row = 2 && num = 3 then
                    (R, true, false, (row_num,col_num), (-1,-1))
                  else num_in_row [R] 0 O num row_type new_coord true
                      (row_num,col_num)
                | O::R::R::tl,R -> if in_a_row = 1 && num = 3 then
                    (R, true, false, (row_num,col_num), (-1,-1))
                  else num_in_row (R::R::tl) 0 O num row_type new_coord true
                      (row_num,col_num)
                | O::R::tl,R -> if in_a_row = 2 && num = 3 then
                    (R, true, false, (row_num,col_num), (-1,-1))
                  else num_in_row (R::tl) 0 O num row_type new_coord true
                      (row_num,col_num)
                | O::R::tl,_ -> num_in_row (R::tl) 0 O num row_type new_coord
                                  true (row_num,col_num)
                | B::tl,B -> num_in_row tl (in_a_row+1) B num row_type new_coord
                               previous_empty previous_coord
                | B::tl,R -> num_in_row tl 1 B num row_type new_coord false
                               (-1,-1)
                | B::tl,_ -> num_in_row tl 1 B num row_type new_coord
                               (not (previous_coord = (-1,-1))) previous_coord
                | R::tl,R -> num_in_row tl (in_a_row+1) R num row_type new_coord
                               previous_empty previous_coord
                | R::tl,B -> num_in_row tl 1 R num row_type new_coord false
                               (-1,-1)
                | R::tl,_ -> num_in_row tl 1 R num row_type new_coord
                               (not (previous_coord = (-1,-1)))
                               previous_coord in

        match board with
        | [] -> []
        | hd::tl -> let (win,next_hole,previous_hole,next_coord,previous_coord)
          = (num_in_row hd 0 O num row_type (row_num,col_num) false (-1,-1)) in

            match win,row_type with
            | O,Row -> check_rows tl Row (row_num+1,0) num
            | O,Column -> check_rows tl Column (0,col_num+1) num
            | O,_ -> check_rows tl row_type (row_num,col_num) num
            | B,Row -> (Win P1,next_hole,previous_hole,next_coord,
                        previous_coord)::(check_rows tl Row (row_num+1,0) num)
            | B,Column -> (Win P1,next_hole,previous_hole,next_coord,
                           previous_coord)::(check_rows tl Column (0,col_num+1)
                                               num)
            | B,_ ->[(Win P1,next_hole,previous_hole,next_coord,previous_coord)]
            | R,Row -> (Win P2,next_hole,previous_hole,next_coord,
                        previous_coord)::(check_rows tl Row (row_num+1,0) num)
            | R,Column -> (Win P2,next_hole,previous_hole,next_coord,
                           previous_coord)::(check_rows tl Column (0,col_num+1)
                                               num)
            | R,_ ->[(Win P2,next_hole,previous_hole,next_coord,previous_coord)]

(* col_start_pos *)
    (* gets a list of coordinates for the starting positions for diagonals along
       the first and last columns *)
    (* Input: the row number, row
              the type of row, row_type
       Output: a list of coordinates for the starting positions for diagonals *)
    let rec col_start_pos (row : int) (row_type : row_type): coordinate list =
        if row = initial_rows - 3 then [] else
        match row_type with
          | LeftDiagonal -> ((row,initial_columns-1))::(col_start_pos (row+1)
                                                          LeftDiagonal)
        | RightDiagonal -> ((row,0))::(col_start_pos (row+1) RightDiagonal)
        | _ -> failwith "not a diagonal"

(* row_start_pos_left *)
    (* gets a list of coordinates for the starting positions for left diagonals
       along the first row *)
    (* Input: the column number, col
       Output: a list of coordinates for the starting positions for diagonals *)
    let rec row_start_pos_left (col : int) : coordinate list =
        if col = 2 then []
        else (0,col)::(row_start_pos_left (col-1))

(* row_start_pos_right *)
    (* gets a list of coordinates for the starting positions for right diagonals
       along the first row *)
    (* Input: the column number, col
       Output: a list of coordinates for the starting positions for diagonals *)
    let rec row_start_pos_right (col : int) : coordinate list =
      if col = (initial_columns-3) then []
      else (0,col)::(row_start_pos_right (col+1))

(* check_left_diags *)
    (* gets information for left diagonals *)
    (* Input: the board, board
              a list of starting positions for diagonals, start_pos
              the number of pieces in a row to look for, num
       Output: a list of matches for left diagonal given the number of pieces in
               a row to look for, containing the status, whether the previous
               and next holes are empty, and the coordinates of the previous and
               next holes *)
(* NESTED HELPERS *)
    (* get_left_diag returns a list of holes corresponding to a left diagonal *)
    let rec check_left_diags (board : hole list list)
        (start_pos : coordinate list) (num : int):
      (status * bool * bool * coordinate * coordinate) list =
      let rec get_left_diag (board : hole list list)
          (row_num,col_num : coordinate) : hole list =
            if row_num = initial_rows || col_num = -1 then []
            else try (List.nth (List.nth board row_num)
                        col_num)::(get_left_diag board (row_num+1,col_num-1))
              with | _ -> failwith "wrong board size" in

        match start_pos with
        | [] -> []
        | hd1::tl1 -> let diag = (check_rows [get_left_diag board hd1]
                                    LeftDiagonal hd1 num) in

            match diag with
            | [] -> check_left_diags board tl1 num
            | hd2::tl2 -> hd2::(check_left_diags board tl1 num)

(* check_right_diags *)
    (* gets information for right diagonals *)
    (* Input: the board, board
              a list of starting positions for diagonals, start_pos
              the number of pieces in a row to look for, num
       Output: a list of matches for right diagonal given the number of pieces
               in a row to look for, containing the status, whether the previous
               and next holes are empty, and the coordinates of the previous and
               next holes *)
(* NESTED HELPERS *)
    (* get_right_diag returns a list of holes corresponding to a right
       diagonal *)
    let rec check_right_diags (board : hole list list)
        (start_pos : coordinate list) (num : int) :
      (status * bool * bool * coordinate * coordinate) list =
      let rec get_right_diag (board : hole list list)
          (row_num,col_num : coordinate) : hole list =
            if row_num = initial_rows || col_num = initial_columns then []
            else try (List.nth (List.nth board row_num)
                        col_num)::(get_right_diag board (row_num+1,col_num+1))
              with | _ -> failwith "wrong board size" in

        match start_pos with
        | [] -> []
        | hd1::tl1 -> let diag = (check_rows [get_right_diag board hd1]
                                    RightDiagonal hd1 num) in

            match diag with
            | [] -> check_right_diags board tl1 num
            | hd2::tl2 -> hd2::(check_right_diags board tl1 num)

(* game_status *)
    (* returns the status of the game at the given state *)
    (* Input: the state of the game, (board,player)
       Output: the status of the game at the given state, either one of the
               player won, it is a draw, or it is ongoing *)
(* NESTED HELPERS *)
    (* check_diags gets information on all the possible diagonal combos *)
    (* is draw checks if the current state is a draw *)
    let game_status (board,player : state) : status =
      let check_diags (board : hole list list) (num : int) :
        (status * bool * bool * coordinate * coordinate) list =
        (check_left_diags board ((col_start_pos
                                    1 LeftDiagonal)@(row_start_pos_left
                                                       (initial_columns-1)))
           num)@(check_right_diags board ((col_start_pos 1
                                             RightDiagonal)@(row_start_pos_right
                                                               0)) num) in
      let rec is_draw (board : hole list list) : bool =
        match board with
        | [] -> failwith "given an empty board"
        | hd::[] -> not (List.mem O hd)
        | hd::tl -> is_draw tl in

        let rows = check_rows board Row (0,0) 4 in

        match rows with
        | (status,_,_,_,_)::_ -> status
        | _ ->
            let columns = check_rows (transpose board) Column (0,0) 4 in

            match columns with
            | (status,_,_,_,_)::_ -> status
            | _ ->
                let diags = check_diags board 4 in

                match diags with
                | (status,_,_,_,_)::_ -> status
                | _ -> if is_draw board then Draw
                else Ongoing player

(* legal_moves_helper *)
    (* produces the set of legal moves at a state, represented as a list *)
    (* Input: the board represented as a list of hole lists, board
              the columns that already have a move (or are full), not_cols
              the list of legal moves, moves
       Output: a list of all possible legal moves that can be made *)
(* NESTED HELPERS *)
    (* row_moves returns any possible moves for that row *)
    (* get_not_cols returns all the column numbers already used for a move *)
    let rec legal_moves_helper (board : hole list list)
        (not_cols : int list) (moves : move list) : move list =
      let rec row_moves (row : hole list) (col_num : int)
          (not_cols : int list): move list =
        match row with
        | [] -> []
        | hd::tl -> if hd = O && not (List.mem col_num not_cols) then
            col_num::(row_moves tl (col_num+1) not_cols)
          else row_moves tl (col_num+1) not_cols in

      let rec get_not_cols (moves : move list) : int list =
        match moves with
        | [] -> []
        | hd::tl -> hd::(get_not_cols tl) in

      match board with
      | [] -> moves
      | hd::tl -> let new_moves = (row_moves hd 0 not_cols)@moves in
        (legal_moves_helper tl (get_not_cols new_moves) new_moves)

(* legal_moves *)
    (* produces the set of legal moves at a state, represented as a list *)
    (* Input: the state of the game, (board,player)
       Output: a list of all possible legal moves that can be made *)
    let legal_moves (board,player : state) : move list =
      let status = game_status (board,player) in
         match status with
         | Win P1 -> []
         | Win P2 -> []
         | Draw -> []
         | _ -> legal_moves_helper board [] []

(* get_coord *)
    (* given a board and a move, gets the coordinates of the move *)
    (* Input: the board, board
              the move, move
       Output: the coordinates corresponding to the input move *)
    let rec get_coord (board : hole list list) (move : move) (row_num : int)
      : coordinate =
        match board with
        | [] -> failwith "not valid move"
        | hd::tl -> if List.nth hd move = O then (row_num,move)
        else get_coord tl move (row_num + 1)

(* next_state *)
    (* given a state and a legal move, yields the next state *)
    (* Input: the state of the game, (board,player)
              the move being made, move
       Output: the state of the game after the new move has been made *)
(* NESTED HELPERS *)
    (* change_row gets each of the new rows after the move *)
    (* change_column gets each of the new column pieces after the move *)
    let next_state (board,player : state) (move : move) : state =
      let rec change_row (board,player : state) (row_num,col_num : coordinate)
          (curr_row : int) : hole list list =
        let rec change_column (column : hole list) (player : which_player)
            (col_num : int) (curr_col : int) : hole list =
                match column with
                | [] -> []
                | hd::tl -> if col_num = curr_col then if player = P1 then
                      [B]@tl else [R]@tl
                else [hd]@(change_column tl player col_num (curr_col+1)) in

            match board with
            | [] -> []
            | hd::tl -> if row_num = curr_row then [change_column hd player
                                                      col_num 0]@tl
            else [hd]@(change_row (tl,player) (row_num,col_num) (curr_row+1)) in

        let coord = get_coord board move 0 in
            if player = P1 then ((change_row (board,player) (coord) 0),P2)
            else ((change_row (board,player) (coord) 0),P1)


  (* SPECIFIC TO HUMAN PLAYERS *)

(* move_of_string *)
    (* for transforming player input into
    * the internal representation of a move *)
    (* Input: a string representing a move, str
       Output: the internal representation of the move *)
    let move_of_string (str : string) : move =
        try
            int_of_string str - 1
        with
        | _ -> -1

    (* SPECIFIC TO AI PLAYERS *)


(* estimate_value *)
    (* estimate the value (for player 1) of a given state *)
    (* Input: the state of the game, (board,player)
       Output: an estimated value of the input state to player 1 *)
(* NESTED HELPERS *)
    (* get_coords converts a list of moves to a list of coordinates *)
    (* unique_moves gets all the unique coordiates from a list of coordinates *)
    (* get_previous determines which moves are possible *)
    (* get_next determines which moves are possible *)
    (* column_win_p1 determines if player 1 can win on a column *)
    (* column_win_p2 determines if player 2 can win on a column *)
    (* get_wins_p1 determines which moves player 1 can win on *)
    (* get_wins_p2 determines which moves player 2 can win on *)
    (* get_next_coords gets a list of the next coordinates for potential wins *)
    (* get_previous_coords gets a list of the previous coordinates for
       potential wins *)
    (* get_next_holes determines which next holes are open *)
    (* get_previous_holes determines which previous holes are open *)
    (* get_status gets a list of all the statuses for a list a moves *)
    (* bool_counter gets the number of ways a player can win and which
       coordinates they can win on *)
    (* bool_counter_helper is a wrapper for bool counter *)
    (* bool_list_to_bool checks if all bools in a list of bools are true *)
    (* estimate_rows_diags estimates the value to player 1 of the current state
       of the game by giving value to potential winning combinations for rows
       and diagoanls *)
    (* get_value converts potential winning combos into value *)
    (* float_of_bools assigns a value for two potential winning combos *)
    (* float_of_player assigns a value to which player's turn it is *)
    let estimate_value (board,player : state) : float =
      let rec get_coords (board : hole list list) (moves : move list)
        : coordinate list =
            match moves with
            | [] -> []
            | hd::tl -> (get_coord board hd 0)::(get_coords board tl) in

        let rec unique_moves (moves : coordinate list) : coordinate list =
            match moves with
            | [] -> []
            | hd::tl -> if List.mem hd tl then unique_moves tl else
                hd::(unique_moves tl) in

      let rec get_previous
          (tup_lst : (status * bool * bool * coordinate * coordinate) list)
          (moves : coordinate list) : bool list =
            match tup_lst with
            | [] -> []
            | (_,_,previous_hole,_,previous_coord)::tl ->
              (previous_hole && List.mem previous_coord moves)::(get_previous
                                                                   tl moves) in

      let rec get_next (tup_lst :
                          (status * bool * bool * coordinate * coordinate) list)
          (moves : coordinate list) : bool list =
            match tup_lst with
            | [] -> []
            | (_,next_hole,_,next_coord,_)::tl ->
              (next_hole && List.mem next_coord moves)::(get_next tl moves) in

      let rec column_win_p1 (cols :
                               (status * bool * bool * coordinate *
                                coordinate) list) : bool list =
            match cols with
            | [] -> []
            | (column,next_hole,_,_,_)::tl ->
              (next_hole && column = Win P1)::(column_win_p1 tl) in

      let rec column_win_p2 (cols : (status * bool * bool * coordinate *
                                     coordinate) list) : bool list =
            match cols with
            | [] -> []
            | (column,next_hole,_,_,_)::tl ->
              (next_hole && column = Win P2)::(column_win_p2 tl) in

      let rec get_wins_p1 (win : bool list) (win_type : (status * bool *
                                                         bool * coordinate *
                                                         coordinate) list) :
        bool list =
            match win,win_type with
            | [],[] -> []
            | hd::tl1,(Win P1,_,_,_,_)::tl2 -> if hd then true::(get_wins_p1
                                                                   tl1 tl2) else
                false::get_wins_p1 tl1 tl2
            | _::tl1,_::tl2 -> false::(get_wins_p1 tl1 tl2)
            | _,_ -> failwith "wrong type" in

      let rec get_wins_p2 (win : bool list) (win_type : (status * bool *
                                                         bool * coordinate *
                                                         coordinate) list) :
        bool list =
            match win,win_type with
            | [],[] -> []
            | hd::tl1,(Win P2,_,_,_,_)::tl2 -> if hd then true::(get_wins_p2
                                                                   tl1 tl2) else
                false::get_wins_p2 tl1 tl2
            | _::tl1,_::tl2 -> false::(get_wins_p2 tl1 tl2)
            | _,_ -> failwith "wrong type" in

      let rec get_next_coords (row : (status * bool * bool * coordinate *
                                      coordinate) list) : coordinate list =
            match row with
            | [] -> []
            | (_,_,_,coord,_)::tl -> coord::(get_next_coords tl) in

      let rec get_previous_coords (row : (status * bool * bool * coordinate *
                                          coordinate) list) : coordinate list =
            match row with
            | [] -> []
            | (_,_,_,_,coord)::tl -> coord::(get_previous_coords tl) in

      let rec get_next_holes (row : (status * bool * bool * coordinate *
                                     coordinate) list) : bool list =
            match row with
            | [] -> []
            | (_,hole,_,_,_)::tl -> hole::(get_next_holes tl) in

      let rec get_previous_holes (row : (status * bool * bool * coordinate *
                                         coordinate) list) : bool list =
            match row with
            | [] -> []
            | (_,hole,_,_,_)::tl -> hole::(get_previous_holes tl) in

      let rec get_status (row : (status * bool * bool * coordinate *
                                 coordinate) list) : status list =
            match row with
            | [] -> []
            | (status,_,_,_,_)::tl -> status::(get_status tl) in

      let rec bool_counter (bool_list : bool list list)
          (move_list : coordinate list list) (wins : int)
          (moves : coordinate list) : int * coordinate list =
        let rec bool_counter_helper (bool_list : bool list)
            (move_list : coordinate list) : int * coordinate list =
                match bool_list,move_list with
                | [],[] -> (0,[])
                | hd1::tl1,hd2::tl2 -> let (num,moves) = bool_counter_helper
                                           tl1 tl2 in
                if hd1 then (1+num,hd2::moves) else (num,moves)
                | _,_ -> failwith "Lists are not of same size" in

            match bool_list,move_list with
            | [],[] -> (wins,moves)
            | hd1::tl1,hd2::tl2 -> let (num,moves1) = bool_counter_helper
                                       hd1 hd2 in
            bool_counter tl1 tl2 (num+wins) (moves1@moves)
            | _,_ -> failwith "Lists are not of same size" in

        let rec bool_list_to_bool (bool_lst : bool list) : bool =
            match bool_lst with
            | [] -> false
            | hd::tl -> hd || bool_list_to_bool tl in

      let rec estimate_rows_diags (rows : status list)
          (left_diagonals : status list) (right_diagonals : status list)
          (next_hole1 : bool list) (next_hole2 : bool list)
          (next_hole3 : bool list) (previous_hole1 : bool list)
          (previous_hole2 : bool list)
          (previous_hole3 : bool list) (player : which_player)
          (value : float) : float =

        let get_value (status : status) (bool1 : bool) (bool2 : bool) :
          float =
                let float_of_bools (bool1 : bool) (bool2 : bool) =
                  if bool1 && bool2 then 3.0 else if bool1 || bool2 then 1.5
                  else 0.0 in

                match status with
                | Win P1 -> float_of_bools bool1 bool2
                | Win P2 -> -. float_of_bools bool1 bool2
                | _ -> 0.0 in

            let float_of_player (player : which_player) =
                match player with
                | P1 -> 1.0
                | P2 -> -1.0 in

        match rows,left_diagonals,right_diagonals,next_hole1,next_hole2,
              next_hole3,previous_hole1,previous_hole2,previous_hole3 with
            | [],[],[],_,_,_,_,_,_ -> value +. float_of_player player
            | hd1::tl1,hd2::tl2,hd3::tl3,hd4::tl4,hd5::tl5,hd6::tl6,hd7::tl7,
              hd8::tl8,hd9::tl9 ->
              estimate_rows_diags tl1 tl2 tl3 tl4 tl5 tl6 tl7 tl8 tl9 player
                value +.
            get_value hd1 hd4 hd7 +. get_value hd2 hd5 hd8
            +. get_value hd3 hd6 hd9
            | [],hd2::tl2,hd3::tl3,_,hd5::tl5,hd6::tl6,_,hd8::tl8,hd9::tl9 ->
            estimate_rows_diags [] tl2 tl3 [] tl5 tl6 [] tl8 tl9 player value +.
            get_value hd2 hd5 hd8 +. get_value hd3 hd6 hd9
            | [],[],hd3::tl3,_,_,hd6::tl6,_,_,hd9::tl9 ->
            estimate_rows_diags [] [] tl3 [] [] tl6 [] [] tl9 player value +.
            get_value hd3 hd6 hd9
            | hd1::tl1,[],hd3::tl3,hd4::tl4,_,hd6::tl6,hd7::tl7,_,hd9::tl9 ->
            estimate_rows_diags tl1 [] tl3 tl4 [] tl6 tl7 [] tl9 player value +.
            get_value hd1 hd4 hd7 +. get_value hd3 hd6 hd9
            | hd1::tl1,[],[],hd4::tl4,_,_,hd7::tl7,_,_ ->
            estimate_rows_diags tl1 [] [] tl4 [] [] tl7 [] [] player value +.
            get_value hd1 hd4 hd7
            | hd1::tl1,hd2::tl2,[],hd4::tl4,hd5::tl5,_,hd7::tl7,hd8::tl8,_ ->
            estimate_rows_diags tl1 tl2 [] tl4 tl5 [] tl7 tl8 [] player value +.
            get_value hd1 hd4 hd7 +. get_value hd2 hd5 hd8
            | [],hd2::tl2,[],_,hd5::tl5,_,_,hd8::tl8,_ ->
            estimate_rows_diags [] tl2 [] [] tl5 [] [] tl8 [] player value +.
            get_value hd2 hd5 hd8
            | _,_,_,_,_,_,_,_,_ -> print_string "something wrong" ; 0.0 in

        let status = game_status (board,player) in

            match status with
            | Win P1 -> max_float
            | Win P2 -> (-1. *. max_float)
            | Draw -> 0.0
            | _ ->
            let moves = legal_moves_helper board [] [] in
            let coords = get_coords board moves in

            let rows = check_rows board Row (0,0) 3 in
            let left_diagonals = check_left_diags board
                ((col_start_pos 1 LeftDiagonal)@(row_start_pos_left
                                                   (initial_columns-1))) 3 in
            let right_diagonals = check_right_diags board
                ((col_start_pos 1 RightDiagonal)@(row_start_pos_right 0)) 3 in
            let columns = check_rows (transpose board) Column (0,0) 3 in

            let row_previous = get_previous rows coords in
            let row_next = get_next rows coords in
            let left_diag_previous = get_previous left_diagonals coords in
            let left_diag_next = get_next left_diagonals coords in
            let right_diag_previous = get_previous right_diagonals coords in
            let right_diag_next = get_next right_diagonals coords in

            let p1_col_win = column_win_p1 columns in
            let p2_col_win = column_win_p2 columns in

            let winning_moves = [get_next_coords columns ; get_previous_coords
                                   rows ;
                                 get_next_coords rows ; get_previous_coords
                                   left_diagonals ;
                                 get_next_coords left_diagonals ;
                                 get_previous_coords right_diagonals ;
                                 get_next_coords right_diagonals] in

            let p1_wins = bool_counter [p1_col_win ;
                get_wins_p1 row_previous rows ; get_wins_p1 row_next rows;
                                        get_wins_p1 left_diag_previous
                                          left_diagonals ; get_wins_p1
                                          left_diag_next left_diagonals ;
                get_wins_p1 right_diag_previous right_diagonals;
                                        get_wins_p1 right_diag_next
                                          right_diagonals]
                winning_moves 0 [] in
            let p2_wins = bool_counter [p2_col_win ;
            get_wins_p2 row_previous rows; get_wins_p2 row_next rows;
                                        get_wins_p2 left_diag_previous
                                          left_diagonals; get_wins_p2
                                          left_diag_next left_diagonals;
                get_wins_p2 right_diag_previous right_diagonals;
                                        get_wins_p2 right_diag_next
                                          right_diagonals]
                winning_moves 0 [] in

            let estimate_p1 = estimate_rows_diags (get_status rows)
                (get_status left_diagonals)
                (get_status right_diagonals) (get_next_holes rows)
                (get_next_holes left_diagonals) (get_next_holes right_diagonals)
                (get_previous_holes rows)
                (get_previous_holes left_diagonals)
                (get_previous_holes right_diagonals) P1 0.0 in

            let estimate_p2 = estimate_rows_diags (get_status rows)
                (get_status left_diagonals)
                (get_status right_diagonals) (get_next_holes rows)
                (get_next_holes left_diagonals) (get_next_holes right_diagonals)
                (get_previous_holes rows)
                (get_previous_holes left_diagonals)
                (get_previous_holes right_diagonals) P2 0.0 in

            match p1_wins,p2_wins,player with
            | (0,_),(0,_),P1 -> estimate_p1
            | (0,_),(0,_),P2 -> estimate_p2
            | (0,_),(num,moves),P1 ->if List.length (unique_moves moves) = 1
              then estimate_p2 +. if bool_list_to_bool p2_col_win
                   then ((float_of_int num)-.1.0)*.1.5 else
                     (float_of_int num)*.1.5
              else (-1. *. max_float) /. 10.0
            | (num,moves),(0,_),P2 ->if List.length (unique_moves moves) = 1
              then estimate_p1 -. if bool_list_to_bool p1_col_win
                   then ((float_of_int num)-.1.0)*.1.5
                   else (float_of_int num)*.1.5
              else max_float /. 10.0
            | _,_,P1 -> max_float /. 10.0
            | _,_,P2 -> (-1. *. max_float) /. 10.0

end ;;

(* TEST CASES *)

open TestGame ;;

  (* string_of_player *)

check_expect (string_of_player P1) "Player 1" ;;
check_expect (string_of_player P2) "Player 2" ;;

  (* string_of_state *)

check_expect (string_of_state ([[O]], P1)) ("\027[33m (1)  (2)  (3)  (4)  (5)" ^
"  (6)  (7)\n\027[0m\027[35m| "
^ "\027[0m\027[32m\027[2mO\027[0m\027[35m |\027[0m");;

(* NOTE *)
(* since we incorporated color printing, testing this is very difficult,
please excuse if this was not exhaustive *)

  (* string_of_move *)

check_expect (string_of_move 1) (":\027[34m\027[1m Column 2\027[0m") ;;
check_expect (string_of_move (-1)) (":\027[34m\027[1m Column 0\027[0m") ;;
check_expect (string_of_move (101)) (":\027[34m\027[1m Column 102\027[0m") ;;

  (* initialize_list *)

check_expect (initialize_list 5 B) [B; B; B; B; B] ;;
check_expect (initialize_list 5 ["|"]) [["|"]; ["|"]; ["|"]; ["|"]; ["|"]] ;;

  (* legal_moves *)

check_expect (legal_moves initial_state) [0; 1; 2; 3; 4; 5; 6] ;;
check_expect (legal_moves (next_state initial_state 0)) [0; 1; 2; 3; 4; 5; 6] ;;
check_expect (legal_moves
([[R; B; O; O; O; O; O]; [R; B; O; O; O; O; O]; [R; B; O; O; O; O; O];
  [R; B; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2)) [] ;; (* Column Win *)
check_expect (legal_moves
  ([[R; B; B; B; B; O; O]; [R; B; O; O; O; O; O]; [R; B; O; O; O; O; O];
    [R; B; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2)) [] ;; (* Row Win *)
check_expect (legal_moves
  ([[R; B; B; B; R; O; O]; [R; B; O; O; B; O; O]; [R; R; O; O; O; B; O];
    [B; B; O; O; O; O; B]; [O; O; O; O; O; O; O]], P1)) [] ;; (* Diagonal Win *)
check_expect (legal_moves
  ([[R; B; B; B; R; O; R]; [R; B; O; O; B; R; O]; [R; R; O; O; R; B; O];
    [B; B; O; R; O; O; R]; [O; O; O; O; O; O; O]], P1)) [] ;; (* Diagonal Win *)
check_expect (legal_moves
([[R; B; B; B; R; B; B]; [R; B; R; B; R; R; B]; [R; R; R; B; R; B; R];
  [B; B; B; R; B; B; R]; [B; R; B; R; B; R; R]], P1)) [] ;; (* Draw *)
check_expect (legal_moves
([[R; B; B; B; R; B; B]; [R; B; R; B; R; R; B]; [R; R; R; B; R; B; R];
  [B; B; B; R; B; B; R]; [O; O; O; O; O; O; O]], P1)) [0;1;2;3;4;5;6] ;;
check_expect (legal_moves
  ([[R; B; B; B; R; B; B]; [R; B; R; B; R; R; B]; [R; R; R; B; R; B; R];
    [B; B; B; R; B; B; R]; [B; O; B; R; B; O; R]], P1)) [1;5] ;;

  (* legal_moves_helper *)

check_expect (legal_moves_helper
[[R; B; B; B; R; B; B]; [R; B; R; B; R; R; B]; [R; R; R; B; R; B; R];
  [B; B; B; R; B; B; R]; [O; O; O; O; O; O; O]] [] []) [0;1;2;3;4;5;6] ;;
check_expect (legal_moves_helper
  [[R; B; B; B; R; B; B]; [R; B; R; B; R; R; B]; [R; R; R; B; R; B; R];
   [B; B; B; R; B; B; R]; [B; O; B; R; B; O; R]] [] []) [1;5] ;;
check_expect (legal_moves_helper
[[O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
 [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]] [] []) [0;1;2;3;4;5;6] ;;

  (* transpose *)

check_expect (transpose [[]]) [] ;;
check_expect (transpose [[O]]) [[O]] ;;
check_expect (transpose [[O ; B]]) [[O] ; [B]] ;;
check_expect (transpose [[O ; B] ; [R ; B] ; [B ; O]])
  [[O ; R ; B] ; [B ; B ; O]] ;;

  (* check_rows *)

check_expect (check_rows [[]] Row (0,0) 3) [] ;;
check_expect (check_rows [[B]] Row (0,0) 3) [] ;;
check_expect (check_rows [[B]] Row (0,0) 1)
  [(Win P1, false, false, (0, 1), (-1, -1))] ;;
check_expect (check_rows [[B;B;B;B]] Row (0,0) 4)
  [(Win P1, false, false, (0, 4), (-1, -1))] ;;
check_expect (check_rows [[B;B;B;B];[R;R;R;R]] Row (0,0) 4)
  [(Win P1, false, false, (0, 4), (-1, -1));
   (Win P2, false, false, (1, 4), (-1, -1))] ;;

  (* col_start_pos *)

check_expect (col_start_pos 1 RightDiagonal) [(1, 0)] ;;
check_expect (col_start_pos 1 LeftDiagonal) [(1, 6)] ;;

  (* row_start_pos_left *)

check_expect (row_start_pos_left (initial_columns-1))
  [(0, 6); (0, 5); (0, 4); (0, 3)] ;;
check_expect (row_start_pos_left (initial_columns-2))
  [(0, 5); (0, 4); (0, 3)] ;;

  (* row_start_pos_right *)

check_expect (row_start_pos_right 0) [(0, 0); (0, 1); (0, 2); (0, 3)] ;;
check_expect (row_start_pos_right 1) [(0, 1); (0, 2); (0, 3)] ;;

  (* check_left_diags *)

check_expect (check_left_diags [[B]] [(0,0)] 1)
  [(Win P1, false, false, (1, -1), (-1, -1))] ;;
check_expect (check_left_diags [[R]] [(0,0)] 1)
  [(Win P2, false, false, (1, -1), (-1, -1))] ;;
check_expect (check_left_diags [[R]] [(0,0)] 2)
  [] ;;
check_expect (check_left_diags [[O;B];[B;O]] [(0,1)] 2)
  [(Win P1, false, false, (2, -1), (-1, -1))] ;;
check_expect (check_left_diags [[B;O];[O;B]] [(0,1)] 2)
  [] ;;

  (* check_right_diags *)

check_expect (check_right_diags [[]] [] 2) [] ;;
check_expect (check_right_diags [[O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
[O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]] [(0,0)] 1)
  [] ;;
check_expect (check_right_diags [[B; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
[O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]] [(0,0)] 1)
  [(Win P1, true, false, (1, 1), (-1, -1))] ;;
check_expect (check_right_diags [[B; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
[O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]] [(0,0)] 12)
  [] ;;
check_expect (check_right_diags [[B; O; O; O; O; O; O]; [O; B; O; O; O; O; O];
[O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]] [(0,0)] 2)
  [(Win P1, true, false, (2, 2), (-1, -1))] ;;

(* game_status *)

check_expect (game_status initial_state) (Ongoing P1) ;;
check_expect (game_status
([[B; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
  [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2))
  (Ongoing P2) ;; (* Ongoing *)
check_expect (game_status
([[O; B; O; O; O; O; O]; [O; B; O; O; O; O; O]; [O; B; O; O; O; O; O];
  [O; B; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2))
  (Win P1) ;; (* Column *)
check_expect (game_status
([[R; O; O; O; O; O; O]; [R; O; O; O; O; O; O]; [R; O; O; O; O; O; O];
  [R; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2))
  (Win P2) ;; (* Column *)
check_expect (game_status
([[R; B; O; O; O; O; O]; [R; B; O; O; O; O; O]; [R; B; O; O; O; O; O];
  [R; B; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2))
  (Win P2) ;; (* Column *)
check_expect (game_status
([[R; B; B; B; B; O; O]; [R; B; O; O; O; O; O]; [R; B; O; O; O; O; O];
  [R; B; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2))
  (Win P1) ;; (* Row *)
check_expect (game_status
([[O; O; O; O; O; O; O]; [R; B; O; O; O; O; O]; [R; B; O; O; O; O; O];
  [R; B; O; O; O; O; O]; [R; B; B; B; B; O; O]], P2))
  (Win P1) ;; (* Row *)
check_expect (game_status
([[R; R; R; R; O; O; O]; [R; B; O; O; O; O; O]; [R; B; O; O; O; O; O];
  [R; B; O; O; O; O; O]; [R; B; B; B; B; O; O]], P2))
  (Win P2) ;; (* Row *)
check_expect (game_status
([[R; B; B; B; R; O; O]; [R; B; O; O; O; O; O]; [R; R; O; O; O; O; O];
  [B; B; O; O; O; O; O]; [O; O; O; O; O; O; O]], P1))
  (Ongoing P1) ;; (* Ongoing *)
check_expect (game_status
([[R; B; B; B; R; O; O]; [R; B; O; O; B; O; O]; [R; R; O; O; O; B; O];
  [B; B; O; O; O; O; B]; [O; O; O; O; O; O; O]], P1))
  (Win P1) ;; (* Right Diagonal *)
check_expect (game_status
([[R; B; B; B; R; O; R]; [R; B; O; O; B; R; O]; [R; R; O; O; R; B; O];
  [B; B; O; R; O; O; R]; [O; O; O; O; O; O; O]], P1))
  (Win P2) ;; (* Left Diagonal *)
  check_expect (game_status
  ([[R; B; B; B; R; B; B]; [R; B; R; B; R; R; B]; [R; R; R; B; R; B; R];
    [B; B; B; R; B; B; R]; [B; R; B; R; B; R; R]], P1))
  (Draw) ;; (* Draw *)

  (* get_coord *)

check_expect (get_coord [[O;O];[O;O]] 1 0) (0, 1) ;;
check_expect (get_coord [[R;B];[O;O]] 1 0) (1, 1) ;;
check_expect (get_coord [[R;B];[O;O]] 0 0) (1, 0) ;;

  (* next_state *)

check_expect (next_state initial_state 0)
  ([[B; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
    [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]],
   P2) ;;
 check_expect (next_state initial_state 2)
   ([[O; O; B; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
     [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]],
    P2) ;;
check_expect (next_state
  ([[O; O; B; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
             [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2) 2)
    ([[O; O; B; O; O; O; O]; [O; O; R; O; O; O; O]; [O; O; O; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]],
     P1) ;;
 check_expect (next_state
   ([[O; O; B; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O];
              [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P1) 2)
     ([[O; O; B; O; O; O; O]; [O; O; B; O; O; O; O]; [O; O; O; O; O; O; O];
       [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]],
      P2) ;;
check_expect (next_state
  ([[O; O; B; O; O; O; R]; [O; O; O; O; O; O; R]; [O; O; O; O; O; O; R];
             [O; O; O; O; O; O; R]; [O; O; O; O; O; O; O]], P1) 6)
    ([[O; O; B; O; O; O; R]; [O; O; O; O; O; O; R]; [O; O; O; O; O; O; R];
      [O; O; O; O; O; O; R]; [O; O; O; O; O; O; B]],
     P2) ;;

  (* move_of_string *)

check_expect (move_of_string "0") (-1) ;;
check_expect (move_of_string "1") 0 ;;
check_expect (move_of_string "3") 2 ;;
check_expect (move_of_string "7") 6 ;;
check_expect (move_of_string "8") 7 ;;
check_expect (move_of_string "-1") (-2) ;;

  (* estimate_value *)

check_expect (estimate_value initial_state) 1.0 ;;
check_expect (estimate_value
    ([[O; O; B; O; O; O; O]; [O; O; B; O; O; O; O]; [O; O; B; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P1)) (max_float/.10.0) ;;
check_expect (estimate_value
    ([[O; O; B; O; O; O; O]; [O; O; B; O; O; O; O]; [O; O; B; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2)) 1.0 ;;
check_expect (estimate_value
    ([[B; O; O; O; O; O; O]; [B; B; B; O; O; O; O]; [O; O; B; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2)) 5.0 ;;
check_expect (estimate_value
    ([[B; O; O; O; O; O; O]; [B; B; B; O; O; O; O]; [B; O; B; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2)) 7.0 ;;
check_expect (estimate_value
    ([[B; O; O; O; O; O; O]; [B; B; B; O; O; O; O]; [B; O; B; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; B; B; B; B]], P2)) max_float ;;
check_expect (estimate_value
    ([[B; O; O; R; O; O; O]; [B; B; B; O; R; R; R]; [B; O; B; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; B; B; B]], P2)) (max_float/.10.0) ;;
check_expect (estimate_value
    ([[B; O; O; O; O; O; O]; [B; B; O; O; R; R; R]; [B; O; B; O; R; R; R];
      [O; O; O; O; O; O; O]; [O; O; O; O; B; B; B]], P2)) 4.0 ;;
check_expect (estimate_value
  ([[O; O; R; O; O; O; O]; [O; O; R; O; O; O; O]; [O; O; R; O; O; O; O];
    [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P2))
  (-1.0*.max_float/.10.0);;
check_expect (estimate_value
    ([[O; O; R; O; O; O; O]; [O; O; R; O; O; O; O]; [O; O; R; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P1)) (-1.0) ;;
check_expect (estimate_value
    ([[R; O; O; O; O; O; O]; [R; R; R; O; O; O; O]; [O; O; R; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P1)) (-5.0) ;;
check_expect (estimate_value
    ([[R; O; O; O; O; O; O]; [R; R; R; O; O; O; O]; [R; O; R; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; O; O; O; O]], P1)) (-7.0) ;;
check_expect (estimate_value
    ([[R; O; O; O; O; O; O]; [R; R; R; O; O; O; O]; [R; O; R; O; O; O; O];
      [O; O; O; O; O; O; O]; [O; O; O; R; R; R; R]], P1)) (-1.0*.max_float) ;;
check_expect (estimate_value
  ([[R; O; O; B; O; O; O]; [R; R; R; O; B; B; B]; [R; O; R; O; O; O; O];
    [O; O; O; O; O; O; O]; [O; O; O; O; R; R; R]], P1))
  (-1.0*.max_float/.10.0);;
check_expect (estimate_value
    ([[R; O; O; O; O; O; O]; [R; R; O; O; B; B; B]; [R; O; R; O; B; B; B];
      [O; O; O; O; O; O; O]; [O; O; O; O; R; R; R]], P1)) (-4.0) ;;
check_expect (estimate_value
    ([[B; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; B; O; O; O; O];
      [O; O; B; B; O; O; O]; [O; O; O; O; O; O; O]], P1)) 4.0 ;;
check_expect (estimate_value
    ([[B; O; O; O; O; O; O]; [O; O; O; O; O; O; O]; [O; O; B; O; O; O; O];
      [O; O; B; B; O; O; O]; [O; O; O; O; O; O; O]], P2)) 2.0 ;;
check_expect (estimate_value
    ([[B; O; O; R; B; B; B]; [O; R; O; O; O; O; O]; [O; O; B; O; O; O; O];
      [O; O; B; B; O; O; O]; [O; O; O; O; O; O; O]], P2)) (-1.0) ;;
check_expect (estimate_value
    ([[O; O; O; O; O; O; O]; [B; B; B; O; O; O; O]; [O; O; O; B; O; B; B];
      [B; O; B; B; O; O; O]; [O; O; O; B; B; O; B]], P2)) 17.0 ;;
check_expect (estimate_value
    ([[O; O; O; O; O; O; O]; [B; B; B; O; O; O; O]; [O; O; O; B; O; B; B];
      [B; O; B; B; O; O; O]; [O; O; O; B; B; O; B]], P1)) 19.0 ;;
check_expect (estimate_value
    ([[R; O; R; R; O; O; O]; [B; B; B; O; O; O; O]; [O; O; O; B; O; B; B];
      [B; O; B; B; O; O; O]; [O; O; O; B; B; O; B]], P2))
  (-1.0 *. max_float /. 10.0) ;;

module ConnectFour = (TestGame : GAME) ;;
