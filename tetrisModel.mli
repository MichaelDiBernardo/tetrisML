(*******************************************************************************
 * The two main objects used in the Tetris game model: A piece, which
 * represents a single thingie that falls towards the bottom of the screen, and
 * the board that tracks the game state.
 * 
 * Author: Michael DiBernardo 
 ******************************************************************************) 

(* class 'tetris_piece'
 *
 * A piece represents a single game piece on the board. The piece is in its own
 * little coordinate system, with the origin at the lower-left. In addition to
 * the run-of-the-mill sort of data you'd expect a piece to have, each piece
 * also maintains its 'skirt', which is a list that represents the lowest
 * 'filled' y-coordinate for each x-coordinate of the piece. This state is
 * maintained to make it easier to drop the thing. 
 *
 * This design borrows heavily from the suggested Stanford CS library
 * implementation of Tetris. I chose to make pieces mutable to make the
 * interface cleaner... I don't like the idea of rotate() producing a new
 * piece, although I suspect it is the better design, especially considering
 * that I'm in a functional language here...
 *
 * The initor takes a list of pairs to initialize the body. If the list is
 * empty or if any of the elements are not pairs, an exception is thrown.
 * 
 * Rotations of a piece are precomputed at init time, so that rotation is a
 * constant-time operation. Sort of... widths and heights are recomputed, but
 * that seems lightweight so I'm not too worried about it. The body of a piece
 * is always sorted to make debugging easier.
 *
 * TODO(mddibern):
 *  - Pairs should be tuples instead of 2-element lists, idiot.
 *)

(* A simple struct to represent positions. *)
type position = { mutable x : int; mutable y : int }

(* Perform some arbitrary binary operation on two positions. *)
val bin_pos_op : (int -> int -> int) -> position -> position -> position

class tetris_piece : int list list -> int -> object
  (* Get the width of this piece. *)
  method get_width : int

  (* Get the height of this piece. *)
  method get_height : int 

  (* Get the skirt of this piece. *)
  method get_skirt : int list

  (* Get the body of this piece: Points are in sorted vector order. *)
  method get_body : int list list

  (* Gets the type id of this piece. Used for colouring in current gui. *)
  method get_type_id : int

  (* Rotate the piece clockwise. This is lightweight, only the height and width are recomputed. *)
  method rotate : unit 

  (* Rotate the piece counterclockwise. *)
  method rotate_cc : unit

  (* Get the "center" of the piece, always skewed towards the top-left. *)
  method get_center : position
end;;

(* class 'tetris_board'
 * 
 * The 'board' class represents the board on which Tetris is played. It contains all of the game logic.
 * Again, this design was heavily influenced by the suggested Tetris design from the Stanford CS library.
 *
 * The board is implemented as a state machine. At each 'tick' of the machine, some input is given and 
 * the machine evolves the game. At each tick, the piece can be moved left, right, and rotated, and it
 * may move down a row if 'evos_per_drop' ticks have passed since the last drop.
 * 
 * See the unittests for example usage scenarios.
 *)

type tetris_grid_cell = Empty | Full of int;;
type lateral_direction = LDNone | Left | Right;;
type rotational_direction = RNone | Clockwise | CounterClockwise;;
type board_input = { mutable hor : lateral_direction; mutable drop : bool; mutable rot : rotational_direction };;
type falling_state = NormalFall | FreeFall;;
type board_state = Falling of falling_state | Landed of int | GameOver;;

class tetris_board : int -> int -> int ->  (unit -> tetris_piece) -> object
  (* Evolve the game by one tick. *)
  method evolve : board_input -> board_state

  (* Get the piece that is currently in play. *)
  method get_current_piece : tetris_piece

  (* Get the next piece that will be played. *)
  method get_next_piece : tetris_piece

  (* Get the board position of the piece that is currently in play. *)
  method get_current_piece_position : position

  (* Get the grid of cells that makes up the board. *)
  method get_grid : tetris_grid_cell array array

  (* Get the height of the board. *)
  method get_height : int

  (* Get the width of the board. *)
  method get_width : int

  (* Get the current score. *)
  method get_score : int

  (* Get the lines completed so far. *)
  method get_lines : int

  (* Get the player level. *)
  method get_level : int

  (* Reset board state so a new game can be played. *)
  method reset : unit
end;;


