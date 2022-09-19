use raylib::prelude::*;
mod engine;
mod test;
use engine::*;

pub enum SoundType {
    Move,
    Capture,
    Check,
    Victory,
    Defeat,
    Draw,
}

pub struct GameState {
    pub valid_moves: Vec<Move>,
    pub selected: Option<Position>,
    pub hovered: Position,
    pub moving: bool,
    pub undo_list: Vec<UndoAction>,
}

impl Default for GameState {
    fn default() -> Self {
        Self {
            valid_moves: Vec::new(),
            selected: None,
            hovered: Position(0, 0),
            moving: false,
            undo_list: Vec::new(),
        }
    }
}

fn main() {
    // Initilize raylib
    let (mut rl, thread) = raylib::init().size(PIECE_SIZE * 8, PIECE_SIZE * 8).title("Chess").build();

    // Sound
    let mut speakers = RaylibAudio::init_audio_device();
    let capture_sound = Sound::load_sound("assets/capture.mp3").expect("Could not load capture sound");
    let move_sound = Sound::load_sound("assets/move.mp3").expect("Could not load move sound");
    let notify_sound = Sound::load_sound("assets/notify.mp3").expect("Could not load notify sound");

    let mut play_sound = |sound_type: SoundType| {
        speakers.play_sound(match sound_type {
            SoundType::Move => &move_sound,
            SoundType::Capture => &capture_sound,
            SoundType::Check | SoundType::Victory | SoundType::Defeat | SoundType::Draw => &notify_sound,
        })
    };

    // Sprites
    let sprite_sheet = rl.load_texture(&thread, "assets/Chess_Pieces_Sprite.png").expect("Could not load piece textures");

    // Game logic
    let mut engine = Engine::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    // TODO Keep state from king in check and draw a red tint on it
    let mut state = GameState {
        valid_moves: engine.calculate_valid_moves(),
        ..Default::default()
    };

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);

        let mouse_position = d.get_mouse_position();
        state.hovered = get_position_form_pixels(mouse_position);

        if d.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
            match state.selected {
                Some(selected) if state.valid_moves.contains(&Move { from: selected, to: state.hovered }) => {
                    state.undo_list.push(engine.make_move(Move { from: selected, to: state.hovered }, &mut play_sound));
                    state.valid_moves = engine.calculate_valid_moves();
                    state.selected = None;
                    state.moving = false;
                }
                _ => match engine.get_piece(state.hovered) {
                    Some(piece) if piece.team == engine.state.turn => {
                        state.selected = Some(state.hovered);
                        state.moving = true;
                    }
                    _ => state.selected = None,
                },
            }
        }
        if d.is_mouse_button_released(MouseButton::MOUSE_LEFT_BUTTON) {
            state.moving = false;
            if let Some(selected) = state.selected {
                let movement = Move { from: selected, to: state.hovered };
                if state.valid_moves.iter().any(|m| m == &movement) {
                    state.undo_list.push(engine.make_move(movement, &mut play_sound));
                    state.valid_moves = engine.calculate_valid_moves();
                    state.selected = None;
                    state.moving = false;
                }
            }
        }
        if d.is_key_pressed(KeyboardKey::KEY_R) {
            engine = Engine::new(STARTING_POSITION);
            state = GameState {
                valid_moves: engine.calculate_valid_moves(),
                ..Default::default()
            };
        } else if d.is_key_pressed(KeyboardKey::KEY_U) {
            if let Some(undo) = state.undo_list.pop() {
                engine.undo_move(undo);
                state.valid_moves = engine.calculate_valid_moves();
                state.selected = None;
                state.moving = false;
            }
        }

        engine.draw_board(&state, &mut d, &sprite_sheet);
    }
}

fn get_position_form_pixels(mouse_position: Vector2) -> Position {
    let file = mouse_position.x as i32 / PIECE_SIZE;
    let rank = mouse_position.y as i32 / PIECE_SIZE;

    Position(i32::min(file, 7), i32::min(rank, 7))
}
