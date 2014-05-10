(*******************************************************************************
 * This file contains the unittests that define the interfaces used for the
 * core objects in the Tetris game. They're all in one file because it makes
 * managing tests easier for the time being. Once I get the hang of unit tests
 * in ocaml, I'll probably do something more complicated.
 * 
 * Author: Michael DiBernardo 
 ******************************************************************************) 
open OUnit
open TetrisModel

(*******************************************************************************
 * Tetris piece unit tests.
 ******************************************************************************)

(* Construct a piece composed of only a single block and rotate it. Nothing
   should change. *)
let test_single_block_piece _ =
  let piece = new tetris_piece [ [0; 0] ] 0 in
  let rec loop i = if i < 5 then (
    assert_equal piece#get_width 1;
    assert_equal piece#get_height 1;
    assert_equal piece#get_skirt [ 0 ];
    assert_equal piece#get_body [ [0; 0] ];
    piece#rotate;
    loop (i+1)
  )
  in
  loop 0;;

(* Construct a two-block piece. Rotating should do stuff now. *)
let test_two_block_piece _ =
  let piece = new tetris_piece [ [0; 0]; [0; 1] ] 0 in
  assert_equal piece#get_width 1;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 0 ];
  assert_equal piece#get_body [ [0; 0]; [0; 1] ];
  piece#rotate;

  assert_equal piece#get_width 2;
  assert_equal piece#get_height 1;
  assert_equal piece#get_skirt [ 0; 0 ];
  assert_equal piece#get_body [ [0; 0]; [1; 0] ];
  piece#rotate;

  assert_equal piece#get_width 1;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 0 ];
  assert_equal piece#get_body [ [0; 0]; [0; 1] ];
  piece#rotate;

  assert_equal piece#get_width 2;
  assert_equal piece#get_height 1;
  assert_equal piece#get_skirt [ 0; 0 ];
  assert_equal piece#get_body [ [0; 0]; [1; 0] ];
  piece#rotate;

  assert_equal piece#get_width 1;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 0 ];
  assert_equal piece#get_body [ [0; 0]; [0; 1] ]

(* Construct a square piece and test that it works. *)
let test_square_piece _ =
  let piece = new tetris_piece [ [0; 0]; [1; 0]; [0; 1]; [1; 1] ] 0 in
  let rec loop i = if i < 5 then (
    assert_equal piece#get_width 2;
    assert_equal piece#get_height 2;
    assert_equal piece#get_skirt [ 0; 0 ];
    assert_equal piece#get_body [ [0; 0]; [0; 1]; [1; 0]; [1; 1] ];
    piece#rotate;
    loop (i+1)
  )
  in
  loop 0;;

(* Construct a left dog and test that it works. You know, this thing:
    oo
     oo *)
let test_left_dog_piece _ =
  let piece = new tetris_piece [ [0; 1]; [1; 1]; [1; 0]; [2; 0] ] 0 in
  assert_equal piece#get_width 3;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 1; 0; 0 ];
  assert_equal piece#get_body [ [0; 1]; [1; 0]; [1; 1]; [2; 0] ];
  piece#rotate; 

  assert_equal piece#get_width 2;
  assert_equal piece#get_height 3;
  assert_equal piece#get_skirt [ 0; 1 ];
  assert_equal piece#get_body [ [0; 0]; [0; 1]; [1; 1]; [1; 2] ];
  piece#rotate;

  assert_equal piece#get_width 3;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 1; 0; 0 ];
  assert_equal piece#get_body [ [0; 1]; [1; 0]; [1; 1]; [2; 0] ];
  piece#rotate; 

  assert_equal piece#get_width 2;
  assert_equal piece#get_height 3;
  assert_equal piece#get_skirt [ 0; 1 ];
  assert_equal piece#get_body [ [0; 0]; [0; 1]; [1; 1]; [1; 2] ];
  piece#rotate;

  assert_equal piece#get_width 3;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 1; 0; 0 ];
  assert_equal piece#get_body [ [0; 1]; [1; 0]; [1; 1]; [2; 0] ]

(* Construct a T shape and test that it works. This is a good case because it
   really has 4 unique conformations. *)
let test_t_piece _ =
  let piece = new tetris_piece [ [0; 0]; [1; 0]; [2; 0]; [1; 1] ] 0 in
  assert_equal piece#get_width 3;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 0; 0; 0 ];
  assert_equal piece#get_body [ [0; 0]; [1; 0]; [1; 1]; [2; 0] ];
  piece#rotate;

  assert_equal piece#get_width 2;
  assert_equal piece#get_height 3;
  assert_equal piece#get_skirt [ 0; 1];
  assert_equal piece#get_body [ [0; 0]; [0; 1]; [0; 2]; [1; 1] ];
  piece#rotate;

  assert_equal piece#get_width 3;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 1; 0; 1];
  assert_equal piece#get_body [ [0; 1]; [1; 0]; [1; 1]; [2; 1] ];
  piece#rotate;

  assert_equal piece#get_width 2;
  assert_equal piece#get_height 3;
  assert_equal piece#get_skirt [ 1; 0];
  assert_equal piece#get_body [ [0; 1]; [1; 0]; [1; 1]; [1; 2] ];
  piece#rotate;

  assert_equal piece#get_width 3;
  assert_equal piece#get_height 2;
  assert_equal piece#get_skirt [ 0; 0; 0 ];
  assert_equal piece#get_body [ [0; 0]; [1; 0]; [1; 1]; [2; 0] ]

(* If you pass an empty body to a piece, it should throw an exception. 
let test_empty_body = 
  try (new tetris_piece []) with
    | Invalid_argument a -> assert_equal a "Can't init piece with an empty body!"
*)


(*******************************************************************************
 * Tetris board unit tests.
 ******************************************************************************)

let repeat_mock_generator body_template = 
  fun () -> new tetris_piece body_template 0

let sequence_mock_generator sequence =
  fun () -> new tetris_piece (Queue.take sequence) 0

let default_height = 4 and default_width = 6 and default_evo = 1;;

let default_test_board gen = 
  new tetris_board default_width default_height default_evo gen

type state_test = { t_in : board_input; expected_state : board_state; expected_pos : position };;

let test_state_sequence board set = 
  let checker x = (
    let actual_state = board#evolve x.t_in in
    (*
    Printf.printf "***";
    Printf.printf "%d, %d vs %d, %d\n" x.expected_pos.x x.expected_pos.y board#get_current_piece_position.x board#get_current_piece_position.y;
    *)
    assert_bool "Expected states not equal" (x.expected_state = actual_state);
    assert_bool "Expected pos not equal: " (x.expected_pos = board#get_current_piece_position)
  )
  in
    List.iter checker set

(* Test where the first piece starts out based on the piece height, board width
   parity, and piece width parity. *)
let test_piece_position_init_even_piece_odd_board _ =
  let height = 4 and width = 5 and evos_per_drop = 1 in
  let board = new tetris_board width height evos_per_drop (repeat_mock_generator [ [0; 0]; [1; 0] ]) in 
  assert_equal board#get_current_piece_position { x=1; y=3 } 

let test_piece_position_init_even_piece_even_board _ =
  let height = 4 and width = 6 and evos_per_drop = 1 in
  let board = new tetris_board width height evos_per_drop (repeat_mock_generator [ [0; 0]; [1; 0] ]) in 
  assert_equal board#get_current_piece_position { x=2; y=3 }

let test_piece_position_init_odd_piece_odd_board _ =
  let height = 4 and width = 5 and evos_per_drop = 1 in
  let board = new tetris_board width height evos_per_drop (repeat_mock_generator [ [0; 0]; [1; 0]; [1; 1]; [2; 0] ]) in 
  assert_equal board#get_current_piece_position { x=1; y=2 }

let test_piece_position_init_odd_piece_even_board _ =
  let height = 4 and width = 6 and evos_per_drop = 1 in
  let board = new tetris_board width height evos_per_drop (repeat_mock_generator [ [0; 0]; [1; 0]; [1; 1]; [2; 0] ]) in 
  assert_equal board#get_current_piece_position { x=1; y=2 }

(* Does the first piece get inited properly? *)
let test_first_piece_inited _ =
  let piece = new tetris_piece [ [0; 0] ] 0 in
  let test_generator = fun () -> piece in
  let board = default_test_board test_generator in 
  assert_equal board#get_current_piece piece 

(* Does the next piece get inited properly? *)
let test_next_piece_inited _ =
  let count = ref 0 and
      first = new tetris_piece [ [0; 0] ] 0 and
      second = new tetris_piece [ [0; 0]; [0; 1] ] 0 in
  let test_generator = fun () -> 
    if !count = 0 then (
      incr count;
      first
    ) else
      second 
  in
  let board = default_test_board test_generator in 
  assert_equal board#get_next_piece second

(* Test that a piece with no one 'controlling' it will fall and hit the floor
   without producing any rows. Then, another piece should start falling.*)
let test_falls_and_hits_floor _ =
  let board = default_test_board (repeat_mock_generator [ [0; 0] ]) in
  let cx = board#get_current_piece_position.x and
      input = { hor = LDNone; drop = false; rot = RNone } in
  let sequence = [
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = cx; y = board#get_height - 2 } };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = cx; y = board#get_height - 3 } };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = cx; y = board#get_height - 4 } };
    { t_in = input; expected_state = Landed(0); expected_pos = { x = cx; y = board#get_height - 4} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = cx; y = board#get_height - 1} }
  ]
  in 
    test_state_sequence board sequence

let test_down_ignores_input_until_fall _ = 
  let down_input = { hor = LDNone; drop = true; rot = RNone } and
      left_input = { hor = Left; drop = false; rot = RNone } and
      right_input = { hor = LDNone; drop = false; rot = Clockwise } in
  let board = default_test_board (repeat_mock_generator [ [0; 0] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = down_input; expected_state = Falling(FreeFall); expected_pos = { x = original_x; y = board#get_height - 2} };
    { t_in = right_input; expected_state = Falling(FreeFall); expected_pos = { x = original_x; y = board#get_height - 3} };
    { t_in = left_input; expected_state = Falling(FreeFall); expected_pos = { x = original_x; y = board#get_height - 4} };
    { t_in = left_input; expected_state = Landed(0); expected_pos = { x = original_x; y = board#get_height - 4} }
  ]
  in
    test_state_sequence board sequence

let test_move_left_with_clipping _ = 
  let input = { hor = Left; drop = false; rot = RNone } and 
      board = default_test_board (repeat_mock_generator [ [0; 0] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x - 1; y = board#get_height - 2} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x - 2; y = board#get_height - 3} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x - 2; y = board#get_height - 4} }
  ]
  in
    test_state_sequence board sequence

let test_evo_rate _ = 
  let input = { hor = Left; drop = false; rot = RNone } and 
      board = new tetris_board default_width default_height 2 (repeat_mock_generator [ [0; 0] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x - 1; y = board#get_height - 1} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x - 2; y = board#get_height - 2} }
  ]
  in
    test_state_sequence board sequence
    
(* Drop a piece that is a single row in length and see that we register a 1-row landing. *)
let test_single_row_detected _ = 
  let input = { hor = LDNone; drop = false; rot = RNone } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [1; 0]; [2; 0]; [3; 0]; [4; 0]; [5; 0] ]) in
  let sequence = [
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = board#get_height - 2} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = board#get_height - 3} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = board#get_height - 4} };
    { t_in = input; expected_state = Landed(1); expected_pos = { x = 0; y = board#get_height - 4} }
  ]
  in
    test_state_sequence board sequence
 
(* Test stacking two inverted Ts:
    o
   ooo
    o
   ooo *)
let test_stacking _ = 
  let input = { hor = LDNone; drop = false; rot = RNone } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [1; 0]; [2; 0]; [1; 1] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = board#get_height - 3} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = board#get_height - 4} };
    { t_in = input; expected_state = Landed(0); expected_pos = { x = original_x; y = board#get_height - 4} };
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = board#get_height - 2} };
    { t_in = input; expected_state = Landed(0); expected_pos = { x = original_x; y = board#get_height - 2} }
  ]
  in
    test_state_sequence board sequence

(* Test left/right clipping on other pieces. Do this by dropping one piece to
   the left, then one to the right, and then 'wiggling' the third left and right
   and seeing that it's stuck, like so:
      3
    1 3 2
    1 3 2
    1   2 *)
let test_left_right_piece_clipping _ = 
  let left_input = { hor = Left; drop = false; rot = RNone } and 
      right_input = { hor = Right; drop = false; rot = RNone } and 
      norm_input = { hor = LDNone; drop = false; rot = RNone } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [0; 1]; [0; 2] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x - 1; y = 0 } };
    { t_in = norm_input; expected_state = Landed(0); expected_pos = { x = original_x - 1; y = 0 } };
    { t_in = norm_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = 1 } };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x + 1; y = 0 } };
    { t_in = norm_input; expected_state = Landed(0); expected_pos = { x = original_x + 1; y = 0 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = 1} };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = 0} };
    { t_in = norm_input; expected_state = Landed(0); expected_pos = { x = original_x; y = 0 } }
  ]
  in
    test_state_sequence board sequence

let test_rotational_clockwise_recentering _ = 
  let rot_input = { hor = LDNone; drop = false; rot = Clockwise } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [0; 1]; [0; 2] ]) in
  let sequence = [
    { t_in = rot_input; expected_state = Falling(NormalFall); expected_pos = { x = 1; y = 1 } };
  ]
  in
    test_state_sequence board sequence

let test_rotational_cclockwise_recentering _ = 
  let rot_input = { hor = LDNone; drop = false; rot = CounterClockwise } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [0; 1]; [0; 2] ]) in
  let sequence = [
    { t_in = rot_input; expected_state = Falling(NormalFall); expected_pos = { x = 1; y = 1 } };
  ]
  in
    test_state_sequence board sequence

(* Test rotational clipping on the wall. *)
let test_rotational_wall_clipping _ = 
  let height = 6 and width = 4 and evos_per_drop = 10 in
  let board = new tetris_board width height evos_per_drop (repeat_mock_generator [ [0; 0]; [0; 1]; [0; 2]; [0; 3] ]) and
      right_input = { hor = Right; drop = false; rot = RNone } and
      rot_input = { hor = LDNone; drop = false; rot = Clockwise } in
  let original_x = board#get_current_piece_position.x in 
  let sequence = [
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x + 1; y = 2 } };
    { t_in = rot_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x + 1; y = 2 } }
  ]
  in
    test_state_sequence board sequence

(* Test rotational clipping on other pieces. *)
let test_rotational_piece_clipping _ = 
  let right_input = { hor = Right; drop = false; rot = RNone } and 
      rot_input = { hor = LDNone; drop = false; rot = Clockwise } and 
      norm_input = { hor = LDNone; drop = false; rot = RNone } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [0; 1]; [0; 2] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x + 1; y = 0 } };
    { t_in = norm_input; expected_state = Landed(0); expected_pos = { x = original_x + 1; y = 0 } };
    { t_in = norm_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = 1} };
    { t_in = rot_input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = 0} }
  ]
  in
    test_state_sequence board sequence

(* Drop a piece that is a single row in length and see that we register a 1-row landing. *)
let test_complete_rows_are_removed _ = 
  let make_pieces_queue () = 
    let q = Queue.create () in
    Queue.push [ [0; 0]; [1; 0]; [2; 0]; [0; 1]; [1; 1]; [2; 1] ] q;
    Queue.push [ [0; 0] ] q;
    Queue.push [ [0; 0]; [1; 0]; [2; 0]; [0; 1]; [1; 1]; [2; 1] ] q;
    Queue.push [ [0; 0] ] q;
    Queue.push [ [0; 0] ] q;
    sequence_mock_generator q
  in 
  let height = 6 and width = 6 and evos_per_drop = 1 in
  let board = new tetris_board width height evos_per_drop (make_pieces_queue ()) and
      left_input = { hor = Left; drop = false; rot = RNone } and
      right_input = { hor = Right; drop = false; rot = RNone } in
  let sequence = [
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 3 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 2 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 1 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 0 } };
    { t_in = left_input; expected_state = Landed(0); expected_pos = { x = 0; y = 0 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 2; y = 5 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 1; y = 4 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 3 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 2 } };
    { t_in = left_input; expected_state = Landed(0); expected_pos = { x = 0; y = 2 } };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = 1; y = 4 } };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = 2; y = 3 } };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = 3; y = 2 } };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = 3; y = 1 } };
    { t_in = right_input; expected_state = Falling(NormalFall); expected_pos = { x = 3; y = 0 } };
    { t_in = left_input; expected_state = Landed(2); expected_pos = { x = 3; y = 0 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 2; y = 5 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 1; y = 4 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 3 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 2 } };
    { t_in = left_input; expected_state = Falling(NormalFall); expected_pos = { x = 0; y = 1 } };
    { t_in = left_input; expected_state = Landed(0); expected_pos = { x = 0; y = 1 } }
  ]
  in
    test_state_sequence board sequence
 
let test_game_over _ = 
  let input = { hor = LDNone; drop = false; rot = RNone } and 
      board = default_test_board (repeat_mock_generator [ [0; 0]; [0; 1]; [0; 2] ]) in
  let original_x = board#get_current_piece_position.x in
  let sequence = [
    { t_in = input; expected_state = Falling(NormalFall); expected_pos = { x = original_x; y = 0 } };
    { t_in = input; expected_state = Landed(0); expected_pos = { x = original_x; y = 0 } };
    { t_in = input; expected_state = GameOver; expected_pos = { x = original_x; y = 1 } }
  ]
  in
    test_state_sequence board sequence

(*** BUILD TETRIS PIECE SUITE ***)
let piece_suite = "Tetris Piece Suite" >::: [
                    "Single block" >:: test_single_block_piece;
                    "Two blocks" >:: test_two_block_piece;
                    "Square piece" >:: test_square_piece;
                    "Left dog piece" >:: test_left_dog_piece;
                    "T piece" >:: test_t_piece
                  ]

(*** BUILD TETRIS BOARD SUITE ***)
let board_suite = "Tetris Board Suite" >::: [
                    "Init piece pos epob" >:: test_piece_position_init_even_piece_odd_board;
                    "Init piece pos epeb" >:: test_piece_position_init_even_piece_even_board;
                    "Init piece pos opob" >:: test_piece_position_init_odd_piece_odd_board;
                    "Init piece pos opob" >:: test_piece_position_init_odd_piece_even_board;
                    "First piece inited" >:: test_first_piece_inited;
                    "Next piece inited" >:: test_next_piece_inited;
                    "Falling to floor" >:: test_falls_and_hits_floor;
                    "Down input cancels others" >:: test_down_ignores_input_until_fall;
                    "Evo rate works" >:: test_evo_rate;
                    "Single row detection" >:: test_single_row_detected;
                    "Simple stacking" >:: test_stacking;
                    "Move left with wall clipping" >:: test_move_left_with_clipping;
                    "Lateral piece clipping" >:: test_left_right_piece_clipping;
                    "Rotational recentering clockwise" >:: test_rotational_clockwise_recentering;
                    "Rotational recentering counterclockwise" >:: test_rotational_cclockwise_recentering;
                    "Rotational wall clipping" >:: test_rotational_wall_clipping;
                    "Rotational piece clipping" >:: test_rotational_wall_clipping;
                    "Nontrivial stacking and row removal" >:: test_complete_rows_are_removed;
                    "Game over" >:: test_game_over
                  ]

let _ =
  run_test_tt_main piece_suite;
  run_test_tt_main board_suite
