#![feature(if_let_guard)]

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
    pub choosing_promotion: Option<Move>,
    pub undo_list: Vec<UndoAction>,
}

impl Default for GameState {
    fn default() -> Self {
        Self {
            choosing_promotion: None,
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

        if let Some(m) = state.choosing_promotion {
            engine.draw_board(&state, &mut d, &sprite_sheet);
            d.draw_rectangle(PIECE_SIZE * 2, PIECE_SIZE * 4, PIECE_SIZE * 4, PIECE_SIZE, Color::WHITE);
            let clicked = d.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON);
            for (i, kind) in [PieceKind::Bishop, PieceKind::Rook, PieceKind::Queen, PieceKind::Knight].iter().enumerate() {
                let (x, y) = Engine::sprite_ofset(&Piece { kind: *kind, team: engine.state.turn });
                let (x2, y2) = ((i + 2) as f32 * PIECE_SIZE as f32, 4_f32 * PIECE_SIZE as f32);
                d.draw_texture_rec(&sprite_sheet, Rectangle::new(x, y, PIECE_SIZE as f32, PIECE_SIZE as f32), Vector2 { x: x2, y: y2 }, Color::WHITE);
                if clicked && mouse_position.x > x2 && mouse_position.x < (x2 + PIECE_SIZE as f32) && mouse_position.y > y2 && mouse_position.y < (y2 + PIECE_SIZE as f32) {
                    state.undo_list.push(engine.make_move(
                        Move {
                            from: m.from,
                            to: m.to,
                            kind: MoveKind::Promote(*kind),
                        },
                        &mut play_sound,
                    ));
                    state.valid_moves = engine.calculate_valid_moves();
                    state.selected = None;
                    state.moving = false;
                    state.choosing_promotion = None;
                }
            }
        } else {
            if d.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
                match state.selected {
                    Some(selected) if let Some(m) = state.valid_moves.iter().find(|m| m.from == selected && m.to == state.hovered) => {
                        if let MoveKind::Promote(_) = m.kind {
                            state.choosing_promotion = Some(*m);
                        } else {
                            state.undo_list.push(engine.make_move(*m, &mut play_sound));
                            state.valid_moves = engine.calculate_valid_moves();
                            state.selected = None;
                            state.moving = false;
                        }
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
                    if let Some(m) = state.valid_moves.iter().find(|m| m.from == selected && m.to == state.hovered) {
                        if let MoveKind::Promote(_) = m.kind {
                            state.choosing_promotion = Some(*m);
                        } else {
                            state.undo_list.push(engine.make_move(*m, &mut play_sound));
                            state.valid_moves = engine.calculate_valid_moves();
                            state.selected = None;
                            state.moving = false;
                        }
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
}

fn get_position_form_pixels(mouse_position: Vector2) -> Position {
    let file = mouse_position.x as i32 / PIECE_SIZE;
    let rank = mouse_position.y as i32 / PIECE_SIZE;

    Position(i32::min(file, 7), i32::min(rank, 7))
}
