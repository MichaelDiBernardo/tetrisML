
(*** TETRIS PIECE HELPER METHODS ***) 

(* This reverses the args to List.nth so that I can supply an index and extract
   on any list rather than vice-versa. *)
let list_ind_extract n l = (List.nth l n)

(* Given a list of lists, examines the nth element in each sublist and
   determines the delta of the smallest and largest values in that position. *)
let compute_sublist_element_delta l n = 
  let nth_element_list = List.map (list_ind_extract n) l in
  (List.fold_left max (List.hd nth_element_list) nth_element_list) -
  (List.fold_left min (List.hd nth_element_list) nth_element_list) + 1

(* Computes the width of a tetris piece given its body. *)
let compute_width l = compute_sublist_element_delta l 0

(* Computes the height of a tetris piece given its body. *)
let compute_height l = compute_sublist_element_delta l 1

(* Rotate the body of a tetris piece 90 degrees clockwise by changing each
   point (x, y) to (y, -x). Note that this might push some transformed y
   coordinates into the negative range, so we then 'normalize' by adding the
   magnitude of the most negative y-value to all y-values. This is horrifically
   inefficient right now, as far as I can tell, but I don't feel so bad because
   it's only done at init time on a handful of pieces. *)
let compute_rotation body = 
  let swap_and_neg pair = match pair with | [x; y] -> [y; -1*x] and
      add_to_y n pair = match pair with | [a; b] -> [a; b + n] in
  let rotated_body = List.map (swap_and_neg) body in
  let most_neg_y = List.fold_left min 0 (List.map (list_ind_extract 1) rotated_body) in
  List.map (add_to_y (-1*most_neg_y)) rotated_body;;

(* Computes the 'skirt' of a piece given its body and the width of the piece.
   This is done by recursively examining each column of the piece, filtering out
   all points in that column, and taking the min y-value. This implementation
   assumes that there are NO empty columns in a piece. In general, pieces should
   be fully-connected anyhow. *)
let compute_skirt body width = 
  let rec build_skirt cur =
    let index = List.length cur in
    if index = width then
      cur
    else 
      let y_values_for_index = List.map (list_ind_extract 1) (List.filter (fun x -> (List.hd x) = index) body) in
      let min_y = List.fold_left min (List.hd y_values_for_index) y_values_for_index in
      build_skirt (min_y :: cur) 
  in 
    (* Because we're consing, the skirt comes out reversed, so rev it. Blech. *)
    List.rev (build_skirt [])

(*** TETRIS PIECE IMPLEMENTATION ***)

type position = { mutable x : int; mutable y : int };;
let bin_pos_op f r1 r2 = { x = (f r1.x r2.x); y = (f r1.y r2.y) }

class tetris_piece (init_body : int list list) (type_id : int) = 
  object (self)
  (* Width of the piece. *)
  val mutable width_ = ( 0 : int )
  (* Height of the piece. *)
  val mutable height_ = ( 0 : int )
  (* The 4 different conformations this piece can take on. *)
  val mutable bodies_list_ = ( Array.make 4 [] : int list list array )
  (* The 4 different skirts that this piece can have. *)
  val mutable skirts_list_ = ( Array.make 4 [] : int list array )
  (* The current conformation that we're in. *)
  val mutable current_rotation_ = ( 0 : int )

  method get_width = 
    width_

  method get_height = 
    height_

  method get_skirt =
    Array.get skirts_list_ current_rotation_

  method get_body = 
    Array.get bodies_list_ current_rotation_
   
  method get_type_id = 
    type_id

  method rotate = 
    current_rotation_ <- (current_rotation_ + 1) mod 4;
    width_ <- compute_width (Array.get bodies_list_ current_rotation_);
    height_ <- compute_height (Array.get bodies_list_ current_rotation_)

  method rotate_cc = 
    for i = 1 to 3 do
      self#rotate
    done

  method get_center =
    { x =  (width_ - 1) / 2; y = height_ / 2 }
    
  initializer 
    let pair_checker x = ((=) (List.length x) 2) in
    if (List.length init_body) = 0 then
      invalid_arg "Can't init piece with an empty body!"
    else if not (List.fold_left (&&) true (List.map pair_checker init_body)) then
      invalid_arg "All elements in body list must be pairs.";
    width_ <- compute_width init_body;
    height_ <- compute_height init_body;
    Array.set bodies_list_ 0 (List.sort compare init_body);
    Array.set skirts_list_ 0 (compute_skirt init_body width_);
    for i = 1 to 3 do 
      Array.set bodies_list_ i (List.sort compare (compute_rotation (Array.get bodies_list_ (i - 1))));
      let new_body = (Array.get bodies_list_ i) in 
      Array.set skirts_list_ i (compute_skirt new_body (compute_width new_body))
    done
end;;

type tetris_grid_cell = Empty | Full of int;;

type lateral_direction = LDNone | Left | Right;;
type rotational_direction = RNone | Clockwise | CounterClockwise;;
type board_input = { mutable hor : lateral_direction; mutable drop : bool; mutable rot : rotational_direction };;

type falling_state = NormalFall | FreeFall;;
type board_state = Falling of falling_state | Landed of int | GameOver;;

let print_elem e = match e with | Empty -> print_string "- " | Full(x) -> print_string "F "

let print_grid g = 
  let print_row r = (Array.iter print_elem r; print_endline "") in
  Array.iter print_row g

class tetris_board (board_width : int) (board_height : int) (evos_per_drop : int) (piece_generator : unit -> tetris_piece ) = 
  object (self) 
  val mutable grid_ = ( Array.make_matrix board_height board_width Empty : tetris_grid_cell array array)
  val mutable current_piece_ = ( (piece_generator ()) : tetris_piece )
  val mutable next_piece_ = ( (piece_generator ()) : tetris_piece )
  val mutable current_piece_position_ = ( { x = 0; y = 0 } : position )
  val mutable heights_ = (Array.make board_width 0 : int array)
  val mutable widths_ = (Array.make board_height 0 : int array)
  val mutable current_state_ = Falling(NormalFall)
  val mutable evo_ticker_ = (0 : int)
  val mutable score_ = (0 : int)
  val mutable lines_ = (0 : int)
  val mutable current_evo_rate_ = (evos_per_drop : int)

  method get_height = 
    board_height

  method get_width = 
    board_width

  method get_grid =
    grid_

  method get_current_piece =
    current_piece_

  method get_next_piece =
    next_piece_

  method get_current_piece_position =
    current_piece_position_

  method evolve (input : board_input) = 
    let new_state = match current_state_ with
    | Falling(fall_type) -> self#move input fall_type
    | Landed(x) -> self#handle_landing
    | GameOver -> GameOver in
    current_state_ <- new_state;
    current_state_

  method get_score =
    score_

  method get_lines = 
    lines_

  method get_level =
    (min (lines_ / 10) 19)

  method reset = 
    grid_ <- ( Array.make_matrix board_height board_width Empty);
    current_piece_ <- (piece_generator ());
    next_piece_ <- (piece_generator ());
    current_piece_position_ <- ({ x = 0; y = 0 });
    heights_ <- (Array.make board_width 0);
    widths_ <- (Array.make board_height 0);
    current_state_ <- Falling(NormalFall);
    evo_ticker_ <- 0;
    score_ <- 0;
    lines_ <- 0;
    current_evo_rate_ <- evos_per_drop
  
  (* Handle a tick where the piece moves. *)
  method private move input fall_type = 
    if (fall_type = NormalFall) then (
      (* First, handle L/R motion. *)
      let x_delta_from_input = fun () -> match input.hor with | Left -> -1 | Right -> 1 | LDNone -> 0 in 
      current_piece_position_ <- (
        let new_pos = { x = current_piece_position_.x + (x_delta_from_input ()); y = current_piece_position_.y } in 
        if (self#bad_clip new_pos) then current_piece_position_ else new_pos);

      if (input.rot <> RNone) then
        self#rotate_and_shift_piece_with_clip input.rot
    );

    if (input.drop = true && fall_type = NormalFall) then print_endline "Dropping!";

    (* Now if it's a downtick, check if we've hit something. *)
    let next_fall_type = match input.drop with | true -> (current_evo_rate_ <- 1; Falling(FreeFall)) | false -> Falling(fall_type) in
    if (self#is_downtick()) then
      if (self#is_clear_below) then (
        current_piece_position_.y <- current_piece_position_.y - 1;
        next_fall_type
      )
      else (
        self#update_grid_after_landing;
        let c = self#handle_possible_row_completes in 
        if c > 0 then (
          lines_ <- lines_ + c;
          score_ <- score_ + (self#compute_score c)
        );
        current_evo_rate_ <- (evos_per_drop - 2 * (self#get_level));
        Landed(c)
      )
    else 
      next_fall_type

  method private compute_score lines_complete =
    (self#get_level + 1) * 
    (match lines_complete with | 1 -> 40 | 2 -> 100 | 3 -> 300 | 4 -> 1200 | _ -> (assert false; 0))

  (* Should the piece fall a row during this tick? *)
  method private is_downtick () =
    evo_ticker_ <- (evo_ticker_ + 1) mod current_evo_rate_;
    evo_ticker_ = 0

  (* Can the piece fall down a row without hitting anything? *)
  method private is_clear_below =
    let t_skirt = Array.map (fun x -> (+) current_piece_position_.y x) (Array.of_list current_piece_#get_skirt) and
        heights = Array.sub heights_ current_piece_position_.x current_piece_#get_width and
        is_clear = ref true in
    Array.iteri (fun i v -> if v <= heights.(i) then is_clear := false) t_skirt; 
    !is_clear

  (* Is the piece currently embedded in either wall? *)
  method private bad_wall_clip pos = 
   (pos.x < 0) || (pos.x + current_piece_#get_width > board_width) || 
   (pos.y < 0) || (pos.y + current_piece_#get_height > board_height)

  (* Is the piece currently overlapping with another piece on the board? *)
  method private bad_piece_clip pos = 
    let clipper piece_point = (
      let translated = match piece_point with | [x; y] -> [|x + pos.x; y + pos.y|] in
      let t_x = translated.(0) and t_y = translated.(1) in 
      (Array.get (Array.get grid_ t_y) t_x) = Empty;
    ) 
    in
      not (List.for_all clipper current_piece_#get_body)
    
  (* Is the piece intersecting the board boundaries or some other piece? *)
  method private bad_clip pos =
    (self#bad_wall_clip pos) || (self#bad_piece_clip pos)

  (* Pieces live in their own little coordinate space, with the origin in the
     bottom-left corner. However, when a piece is rotated, we often want to rotate
     it about its center, which involves "shifting" the coordinate space on the
     board. This method does all that, and if the rotated/shifted piece intersects
     with some other piece or with the board, it undoes the move. *)
  method private rotate_and_shift_piece_with_clip rot_type =
    let old_center = current_piece_#get_center in
    if rot_type = Clockwise then current_piece_#rotate else current_piece_#rotate_cc;
    let new_center = current_piece_#get_center in
    let center_diff = bin_pos_op (-) old_center new_center in 
    let new_pos = bin_pos_op (+) center_diff current_piece_position_ in
    if self#bad_clip new_pos then
      match rot_type with | Clockwise -> current_piece_#rotate_cc 
                          | CounterClockwise -> current_piece_#rotate
    else
      current_piece_position_ <- new_pos
    
  (* Updates the grid with Full squares where the piece landed. *)
  method private update_grid_after_landing = 
    let updater piece_point = (
      let translated = match piece_point with | [x; y] -> [|x + current_piece_position_.x; y + current_piece_position_.y|] in 
      let t_x = translated.(0) and t_y = translated.(1) in 
      (* Printf.printf "\nSetting grid at (%d, %d) to be full.\n" translated.(x) translated.(y); *)
      print_endline "\tblitpoint";
      Array.set grid_.(t_y) t_x (Full current_piece_#get_type_id);
      (* Printf.printf "Incrementing widths at index %d.\n" translated.(y); *)
      Array.set widths_ t_y (widths_.(t_y) + 1);
      if (t_y + 1 > heights_.(t_x)) then
        Array.set heights_ t_x (t_y + 1)
    ) 
    in (
      print_endline "Fixing piece to grid"; 
      List.iter updater current_piece_#get_body
    )

  (* Checks for complete rows and removes them. *)
  method private handle_possible_row_completes =
    let rows_complete = ref 0 in (
      for i = board_height - 1 downto 0 do
        if widths_.(i) = board_width then (
          if (i <> board_height - 1) then (
            Array.blit grid_ (i + 1) grid_ i (board_height - i - 1);
            Array.blit widths_ (i + 1) widths_ i (board_height - i - 1)
          );
          incr rows_complete
        )
      done;
      if (!rows_complete > 0) then self#recompute_heights;
      !rows_complete
    )

  (* Recomputes the "heights" array based on the grid. *)
  method private recompute_heights =
    for c = 0 to board_width - 1 do
      let rec new_height i =
        if (i < 0) then 0
        else if (Array.get grid_.(i) c) <> Empty then (i + 1)
        else new_height (i - 1)
      in 
        Array.set heights_ c (new_height (board_height - 1))
    done

  (* After a landing state, shuffle in the next piece. If we intersect with
     anything, it's game over. *)
  method private handle_landing = 
    print_endline "Handling landing"; 
    print_grid grid_;
    current_piece_ <- next_piece_;
    next_piece_ <- piece_generator ();
    current_piece_position_ <- self#init_piece_position;
    match (self#bad_piece_clip current_piece_position_) with
      | true -> GameOver
      | false -> Falling(NormalFall)

  (* Where should a piece start falling from? *)
  method private init_piece_position = 
    { x = (board_width - current_piece_#get_width) / 2; 
      y = board_height - current_piece_#get_height } 

  initializer
    current_piece_position_ <- self#init_piece_position
end;;
