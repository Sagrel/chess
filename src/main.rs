use raylib::prelude::*;
mod engine;
use engine::*;

fn main() {
    let (mut rl, thread) = raylib::init().size(piece_size * 8, piece_size * 8).title("Chess").build();

    let mut engine = Engine::new(&mut rl, &thread);
    let mut valid_moves = engine.calculate_valid_moves();

    

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);

        let mouse_position = d.get_mouse_position();
        engine.hovered = get_position_form_pixels(mouse_position);

        if d.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
            let clicked_square = engine.get_piece(engine.hovered);

            match clicked_square {
                Some(piece) if piece.team == engine.turn => {
                    engine.selected = Some(engine.hovered);
                    engine.moving = true;
                }
                _ => engine.selected = None,
            }
        }
        if d.is_mouse_button_released(MouseButton::MOUSE_LEFT_BUTTON) {
            engine.moving = false;
            if let Some(selected) = engine.selected {
                let movement = Move { from: selected, to: engine.hovered };
                if valid_moves.iter().any(|m| m == &movement) {
                    engine.make_move(movement);
                    valid_moves = engine.calculate_valid_moves();
                }
            }
        }
        if d.is_key_pressed(KeyboardKey::KEY_R) {
            //engine = Engine::new(&mut rl, &thread);
        }

        engine.draw_board(&mut d, &valid_moves);
    }
}

fn get_position_form_pixels(mouse_position: Vector2) -> Position {
    let file = mouse_position.x as i32 / piece_size;
    let rank = mouse_position.y as i32 / piece_size;

    Position(i32::min(file, 7), i32::min(rank, 7))
}
