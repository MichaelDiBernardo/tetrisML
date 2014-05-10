open TetrisModel

(*
let b = ref 0;;
let rec sna ~value:vv = (
  b := (!b + 1) mod 3;
  Glut.timerFunc ~ms:1 ~cb:sna ~value:2;
  match !b with | 0 -> GlClear.color(1., 0., 0.)
                | 1 -> GlClear.color (0., 1., 0.)
                | 2 -> GlClear.color (0., 0., 1.);
)
  (*Glut.timerFunc ~ms:frame_rate_ms ~cb:sna ~value:2; *)
*)

let b_input = { hor = LDNone; drop = false; rot = RNone }

let board = 
  let dimension = (10, 20) and 
  make_generator() = 
    let pieces = 
      [|
      new tetris_piece [ [0; 0]; [0; 1]; [1; 0]; [1; 1] ] 0;
      new tetris_piece [ [0; 0]; [1; 0]; [2; 0]; [3; 0] ] 1;
      new tetris_piece [ [0; 1]; [1; 1]; [2; 1]; [1; 0] ] 2;
      new tetris_piece [ [0; 0]; [1; 0]; [2; 0]; [2; 1] ] 3;
      new tetris_piece [ [0; 0]; [1; 0]; [2; 0]; [0; 1] ] 4;
      new tetris_piece [ [0; 1]; [1; 1]; [1; 0]; [2; 0] ] 5;
      new tetris_piece [ [0; 0]; [1; 0]; [1; 1]; [2; 1] ] 6|] in
    fun () -> (Oo.copy pieces.(Random.int 7)) in
  new tetris_board (fst dimension) (snd dimension) 40 (make_generator())

let draw_string font step s x y =
  let offset = ref 0. in
  let draw_and_step c = 
    GlPix.raster_pos ~x:(x +. !offset) ~y:y ();
    offset := !offset +. step;
    Glut.bitmapCharacter font (int_of_char c)
  in
    String.iter draw_and_step s

let change_and_clamp tuple grade =
  match tuple with 
    | (x, y, z) -> (grade *. x, grade *. y, grade *. z)
 
let lighter x = change_and_clamp x 1.5

let darker x = change_and_clamp x 0.5 

let draw_box c_norm (lower_left_x, lower_left_y) = 
  let l_x = float_of_int lower_left_x and l_y = float_of_int lower_left_y in
  let c_lighter = lighter c_norm and c_darker = darker c_norm in
  GlDraw.color (0.0, 0.0, 0.0);
  GlMat.push();
  GlMat.translate ~x:(1.1 *. l_x) ~y:(1.1 *. l_y) ();
  GlDraw.begins `quads;
    List.iter GlDraw.vertex2 [0., 0.; 1.2, 0.; 1.2, 1.2; 0., 1.2];
  GlDraw.ends ();
  GlMat.pop();
  GlMat.push();
  GlMat.translate ~x:(l_x +. 0.1 *. (l_x +. 1.)) ~y:(l_y +. 0.1 *. (l_y +. 1.)) ();
  GlDraw.color c_norm;
  GlDraw.begins `quads;
    List.iter GlDraw.vertex2 [0., 0.; 1., 0.; 1., 1.; 0., 1.];
  GlDraw.ends ();
  GlDraw.color c_darker;
  GlDraw.begins `quads;
    List.iter GlDraw.vertex2 [0., 0.; 0.05, 0.05; 0.95, 0.05; 1.0, 0.0];
    List.iter GlDraw.vertex2 [1.0, 0.0; 0.95, 0.05; 0.95, 0.95; 1.0, 1.0];
    GlDraw.color c_lighter;
    List.iter GlDraw.vertex2 [0.0, 0.0; 0.05, 0.05; 0.05, 0.95; 0.0, 1.0];
    List.iter GlDraw.vertex2 [0.0, 1.0; 0.05, 0.95; 0.95, 0.95; 1.0, 1.0];
  GlDraw.ends ();
  GlMat.pop()

let vertex_of_body_cell bc = 
  match bc with | [a; b] -> (a, b)

let color_for_type t = match t with 
  | 0 -> (0.7, 0.0, 0.0)
  | 1 -> (0.0, 0.7, 0.0)
  | 2 -> (0.0, 0.0, 0.7)
  | 3 -> (0.8, 0.8, 0.0)
  | 4 -> (0.8, 0.4, 0.0)
  | 5 -> (0.7, 0.0, 0.7)
  | 6 -> (0.0, 0.7, 0.7)
 
let draw_piece pos piece = 
  let body = piece#get_body in 
  let translated_body = List.map (fun cell -> match cell with | [x; y] -> [pos.x + x; pos.y + y]) body in
  let col = color_for_type piece#get_type_id in
    List.iter (draw_box col) (List.map vertex_of_body_cell translated_body)

let draw_arena () = 
  GlDraw.begins `quads;
    GlDraw.color (0.9, 0.9, 0.9);
    List.iter GlDraw.vertex2 [0., 0.; 1., 0.; 1., 1.; 0., 1.];
  GlDraw.ends ()
  
let draw_background () = 
  GlClear.color (0.6, 0.6, 0.6);
  GlClear.clear [ `color ];
  GlMat.push();
  GlMat.translate ~x:5. ~y:5. ();
  GlMat.scale ~x:(1.1*.10.) ~y:(1.1*.20.) ();
  draw_arena();
  GlMat.pop()

let draw_board board =
  GlMat.push();
  GlMat.translate ~x:5. ~y:5. ();
  draw_piece board#get_current_piece_position board#get_current_piece;
  for i = 0 to board#get_width - 1 do
    for j = 0 to board#get_height -1 do
      ignore(match board#get_grid.(j).(i) with 
             | Empty -> () 
             | Full(x) -> draw_box (color_for_type x) (i, j))
    done
  done;
  GlMat.pop();
  draw_piece { x = 20; y = 24 } board#get_next_piece;
  GlDraw.color (0.0, 0.0, 0.0);
  GlMat.push();
    draw_string Glut.BITMAP_9_BY_15 0.7 "Score" 20. 21.;
    draw_string Glut.BITMAP_9_BY_15 0.7 (string_of_int board#get_score) 20. 20.;
    draw_string Glut.BITMAP_9_BY_15 0.7 "Level" 20. 17.;
    draw_string Glut.BITMAP_9_BY_15 0.7 (string_of_int board#get_level) 20. 16.;
    draw_string Glut.BITMAP_9_BY_15 0.7 "Lines" 20. 13.;
    draw_string Glut.BITMAP_9_BY_15 0.7 (string_of_int board#get_lines) 20. 12.;
  GlMat.pop()

let ingame_keyboard ~key ~x ~y =
  match key with 
    | 44 -> (b_input.hor <- Left)
    | 46 -> (b_input.hor <- Right)
    | 122 -> (b_input.rot <- CounterClockwise)
    | 120 -> (b_input.rot <- Clockwise)
    | 32 -> (b_input.drop <- true)
    | _ -> ()

let ingame_render () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0., 30.) ~y:(0., 30.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  draw_background();
  draw_board board;
  Gl.flush ();
  Glut.swapBuffers ()

let startscreen_render() = 
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0., 40.) ~y:(0., 40.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.color (0.0, 0.0, 0.0);
  GlClear.clear [ `color ];
  List.iter (draw_box (0.6, 0.0, 0.0)) [5, 26; 5, 27; 5, 28; 5, 29; 3, 30; 4, 30; 5, 30; 6, 30; 7, 30];
  List.iter (draw_box (0.0, 0.6, 0.0)) [9, 26; 9, 27; 9, 28; 9, 29; 9, 30; 10, 26; 11, 26; 10, 28; 11, 28; 10, 30; 11, 30];
  List.iter (draw_box (0.0, 0.0, 0.6)) [15, 26; 15, 27; 15, 28; 15, 29; 13, 30; 14, 30; 15, 30; 16, 30; 17, 30];
  List.iter (draw_box (0.0, 0.8, 0.8)) [19, 26; 19, 27; 19, 28; 19, 29; 19, 30; 20, 28; 21, 29; 20, 30; 21, 30; 21, 26; 21, 27];
  List.iter (draw_box (0.6, 0.0, 0.6)) [24, 26; 24, 27; 24, 28; 24, 29; 24, 30; 23, 30; 25, 30; 23, 26; 25, 26];
  List.iter (draw_box (0.6, 0.6, 0.0)) [27, 26; 29, 27; 27, 28; 27, 29; 27, 30; 28, 26; 29, 26; 28, 28; 29, 28; 28, 30; 29, 30];
  GlDraw.color (1.0, 1.0, 1.0);
  draw_string Glut.BITMAP_HELVETICA_18 0.8 "Press any key to start" 10. 17.;
  Gl.flush ();
  Glut.swapBuffers ()

let endgame_render() = 
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0., 40.) ~y:(0., 40.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.color (0.0, 0.0, 0.0);
  GlClear.clear [ `color ];
  List.iter (draw_box (0.6, 0.0, 0.0)) [5, 26; 5, 27; 5, 28; 5, 29; 3, 30; 4, 30; 5, 30; 6, 30; 7, 30];
  List.iter (draw_box (0.0, 0.6, 0.0)) [9, 26; 9, 27; 9, 28; 9, 29; 9, 30; 10, 26; 11, 26; 10, 28; 11, 28; 10, 30; 11, 30];
  List.iter (draw_box (0.0, 0.0, 0.6)) [15, 26; 15, 27; 15, 28; 15, 29; 13, 30; 14, 30; 15, 30; 16, 30; 17, 30];
  List.iter (draw_box (0.0, 0.8, 0.8)) [19, 26; 19, 27; 19, 28; 19, 29; 19, 30; 20, 28; 21, 29; 20, 30; 21, 30; 21, 26; 21, 27];
  List.iter (draw_box (0.6, 0.0, 0.6)) [24, 26; 24, 27; 24, 28; 24, 29; 24, 30; 23, 30; 25, 30; 23, 26; 25, 26];
  List.iter (draw_box (0.6, 0.6, 0.0)) [27, 26; 29, 27; 27, 28; 27, 29; 27, 30; 28, 26; 29, 26; 28, 28; 29, 28; 28, 30; 29, 30];
  GlDraw.color (1.0, 1.0, 1.0);
  draw_string Glut.BITMAP_HELVETICA_18 0.8 "Press any key to start" 10. 17.;
  Gl.flush ();
  Glut.swapBuffers ()
  
let endgame_render() = 
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0., 40.) ~y:(0., 40.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.color (0.0, 0.0, 0.0);
  GlClear.clear [ `color ];
  GlDraw.color (1.0, 1.0, 1.0);
  draw_string Glut.BITMAP_HELVETICA_18 0.8 "GAME OVER" 14. 17.;
  Gl.flush ();
  Glut.swapBuffers ()

type mode_transition = Intro | StartGame | EndGame

let rec modeswitch t = 
  let rec ingame_ticker ~value:board = 
    let game_state = board#evolve b_input in begin
      ignore(match game_state with
        | GameOver -> modeswitch(EndGame)
        | Landed(lines) -> b_input.drop <- false
        | Falling(ftype) -> ());
      b_input.rot <- RNone;
      b_input.hor <- LDNone;
      b_input.drop <- false;
      Glut.timerFunc ~ms:20 ~cb:ingame_ticker ~value:board;
      Glut.postRedisplay()
    end
  in
  match t with 
    | Intro -> (
        Glut.displayFunc ~cb:startscreen_render;
        Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> modeswitch(StartGame));
        Glut.postRedisplay ()
      )
    | StartGame -> (
        Glut.displayFunc ~cb:ingame_render;
        Glut.timerFunc ~ms:1000 ~cb:ingame_ticker ~value:board;
        Glut.keyboardFunc ~cb:ingame_keyboard;
        Glut.postRedisplay ()
      )
    | EndGame -> (
        Glut.displayFunc ~cb:endgame_render;
        Glut.timerFunc ~ms:2000 ~cb:(fun ~value -> (board#reset; modeswitch(Intro)) ) ~value:0;
        Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> ());
        Glut.postRedisplay ()
      )

let _ =
  let argv' = Glut.init Sys.argv in
  Random.self_init();
  Glut.initDisplayMode ~double_buffer:true ();
  Glut.initWindowSize ~w:800 ~h:600;
  Glut.initWindowPosition ~x:0 ~y:0;
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  GlDraw.shade_model `smooth;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  modeswitch(Intro);
  Glut.mainLoop ()
