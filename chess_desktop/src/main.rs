#![feature(if_let_guard)]

use raylib::prelude::*;

use chess_engine::*;

use raylib::{
    ffi::Vector2,
    prelude::{Color, RaylibDraw, RaylibDrawHandle, Rectangle},
    texture::Texture2D,
};

const TRANSPARENT: Color = Color { r: 255, g: 255, b: 255, a: 100 };
const WHITE: Color = Color { r: 255, g: 255, b: 255, a: 255 };
const LIGHT: Color = Color { r: 235, g: 210, b: 183, a: 255 };
const DARK: Color = Color { r: 148, g: 102, b: 83, a: 255 };
const GREEN: Color = Color { r: 130, g: 151, b: 105, a: 180 };

// TODO move everything into GameState and then just in main create it and run everything in a loop
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

pub fn draw_board(engine: &Engine, state: &GameState, d: &mut RaylibDrawHandle, sprite_sheet: &Texture2D) {
    draw_checker_pattern(d);
    draw_pieces(engine, state, d, sprite_sheet);
    draw_active_piece(engine, state, d, sprite_sheet);
}

fn draw_checker_pattern(d: &mut RaylibDrawHandle) {
    for file in 0..8 {
        for rank in 0..8 {
            d.draw_rectangle(file * PIECE_SIZE, rank * PIECE_SIZE, PIECE_SIZE, PIECE_SIZE, if (file + rank) % 2 == 0 { LIGHT } else { DARK })
        }
    }
}

fn draw_pieces(engine: &Engine, state: &GameState, d: &mut RaylibDrawHandle, sprite_sheet: &Texture2D) {
    for file in 0..8 {
        for rank in 0..8 {
            if let Some(piece) = engine.get_piece(Position(file, rank)) {
                let (x, y) = sprite_ofset(piece);

                let color = if state.selected == Some(Position(file, rank)) && state.moving { TRANSPARENT } else { WHITE };
                d.draw_texture_rec(
                    sprite_sheet,
                    Rectangle::new(x, y, PIECE_SIZE as f32, PIECE_SIZE as f32),
                    Vector2 {
                        x: file as f32 * PIECE_SIZE as f32,
                        y: rank as f32 * PIECE_SIZE as f32,
                    },
                    color,
                );
            }
        }
    }
}

fn draw_active_piece(engine: &Engine, state: &GameState, d: &mut RaylibDrawHandle, sprite_sheet: &Texture2D) {
    if let Some(selected) = state.selected {
        for m in &state.valid_moves {
            if m.from == selected {
                if state.hovered == m.to {
                    d.draw_rectangle(m.to.0 * PIECE_SIZE, m.to.1 * PIECE_SIZE, PIECE_SIZE, PIECE_SIZE, GREEN);
                } else if engine.get_piece(m.to).is_some() {
                    let mut x = m.to.0 as f32 * PIECE_SIZE as f32;
                    let mut y = m.to.1 as f32 * PIECE_SIZE as f32;
                    let triangle_size = PIECE_SIZE as f32 / 4_f32;
                    let empty_space = triangle_size * 3_f32;
                    // Top left
                    d.draw_triangle(Vector2 { x: x + triangle_size, y }, Vector2 { x, y }, Vector2 { x, y: y + triangle_size }, GREEN);
                    x += empty_space;
                    // Top right
                    d.draw_triangle(
                        Vector2 { x: x + triangle_size, y },
                        Vector2 { x, y },
                        Vector2 {
                            x: x + triangle_size,
                            y: y + triangle_size,
                        },
                        GREEN,
                    );
                    y += empty_space;
                    // Botton right
                    d.draw_triangle(
                        Vector2 { x: x + triangle_size, y },
                        Vector2 { x, y: y + triangle_size },
                        Vector2 {
                            x: x + triangle_size,
                            y: y + triangle_size,
                        },
                        GREEN,
                    );
                    x -= empty_space;
                    // Botton left
                    d.draw_triangle(
                        Vector2 { x, y },
                        Vector2 { x, y: y + triangle_size },
                        Vector2 {
                            x: x + triangle_size,
                            y: y + triangle_size,
                        },
                        GREEN,
                    );
                } else {
                    d.draw_circle(m.to.0 * PIECE_SIZE + (PIECE_SIZE / 2), m.to.1 * PIECE_SIZE + (PIECE_SIZE / 2), (PIECE_SIZE / 8) as f32, GREEN);
                }
            }
        }

        if state.moving {
            let mp = d.get_mouse_position();
            let position = Vector2 {
                x: mp.x - (PIECE_SIZE / 2) as f32,
                y: mp.y - (PIECE_SIZE / 2) as f32,
            };

            let (x, y) = sprite_ofset(engine.get_piece(selected).as_ref().unwrap());
            d.draw_texture_rec(sprite_sheet, Rectangle::new(x, y, PIECE_SIZE as f32, PIECE_SIZE as f32), position, WHITE);
        }
    }
}

fn sprite_ofset(piece: &Piece) -> (f32, f32) {
    let x = PIECE_SIZE
        * match piece.kind {
            PieceKind::Pawn => 5,
            PieceKind::Rook => 4,
            PieceKind::Knight => 3,
            PieceKind::Bishop => 2,
            PieceKind::Queen => 1,
            PieceKind::King => 0,
        };
    let y = if piece.team == Team::White { 0 } else { PIECE_SIZE };
    (x as f32, y as f32)
}

fn play_sound(engine: &Engine, speakers: &mut RaylibAudio, m: &Move, capture: &Sound, movement: &Sound) {
    speakers.play_sound(if m.kind == MoveKind::Enpasant || engine.get_piece(m.to).is_some() { capture } else { movement })
}

fn make_move(engine: &mut Engine, state: &mut GameState, speakers: &mut RaylibAudio, m: &Move, capture: &Sound, movement: &Sound, notify: &Sound) {
    play_sound(engine, speakers, m, capture, movement);

    state.undo_list.push(engine.make_move(*m));
    state.valid_moves = engine.calculate_valid_moves();
    state.selected = None;
    state.moving = false;
    state.choosing_promotion = None;

    // TODO Handle sounds for drowned king, check and ckeckmate
    if state.valid_moves.is_empty() {
        speakers.play_sound(notify);
    }
}

fn main() {
    // Initilize raylib
    let (mut rl, thread) = raylib::init().size(PIECE_SIZE * 8, PIECE_SIZE * 8).title("Chess").build();

    // Sound
    let mut speakers = RaylibAudio::init_audio_device();
    let capture = &Sound::load_sound("assets/capture.mp3").expect("Could not load capture sound");
    let movement = &Sound::load_sound("assets/move.mp3").expect("Could not load move sound");
    let notify = &Sound::load_sound("assets/notify.mp3").expect("Could not load notify sound");

    // Sprites
    let sprite_sheet = rl.load_texture(&thread, "assets/Chess_Pieces_Sprite.png").expect("Could not load piece textures");

    // Game logic
    let mut engine = Engine::new(STARTING_POSITION);

    // TODO Keep state from king in check and draw a red tint on it
    let mut state = GameState {
        valid_moves: engine.calculate_valid_moves(),
        ..Default::default()
    };

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);

        let mouse_position = d.get_mouse_position();
        state.hovered = get_position_form_pixels(mouse_position.into());

        if let Some(m) = state.choosing_promotion {
            draw_board(&engine, &state, &mut d, &sprite_sheet);
            d.draw_rectangle(PIECE_SIZE * 2, PIECE_SIZE * 4, PIECE_SIZE * 4, PIECE_SIZE, Color::WHITE);
            let clicked = d.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON);
            for (i, kind) in [PieceKind::Bishop, PieceKind::Rook, PieceKind::Queen, PieceKind::Knight].iter().enumerate() {
                let (x, y) = sprite_ofset(&Piece { kind: *kind, team: engine.state.turn });
                let (x2, y2) = ((i + 2) as f32 * PIECE_SIZE as f32, 4_f32 * PIECE_SIZE as f32);
                d.draw_texture_rec(&sprite_sheet, Rectangle::new(x, y, PIECE_SIZE as f32, PIECE_SIZE as f32), Vector2 { x: x2, y: y2 }, Color::WHITE);
                if clicked && mouse_position.x > x2 && mouse_position.x < (x2 + PIECE_SIZE as f32) && mouse_position.y > y2 && mouse_position.y < (y2 + PIECE_SIZE as f32) {
                    make_move(
                        &mut engine,
                        &mut state,
                        &mut speakers,
                        &Move {
                            from: m.from,
                            to: m.to,
                            kind: MoveKind::Promote(*kind),
                        },
                        capture,
                        movement,
                        notify,
                    );
                }
            }
        } else {
            if d.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
                match state.selected {
                    Some(selected) if let Some(&m) = state.valid_moves.iter().find(|m| m.from == selected && m.to == state.hovered) => {
                        if let MoveKind::Promote(_) = m.kind {
                            state.choosing_promotion = Some(m);
                        } else {
                            make_move(&mut engine, &mut state, &mut speakers, &m, capture, movement, notify);
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
                    if let Some(&m) = state.valid_moves.iter().find(|m| m.from == selected && m.to == state.hovered) {
                        if let MoveKind::Promote(_) = m.kind {
                            state.choosing_promotion = Some(m);
                        } else {
                            make_move(&mut engine, &mut state, &mut speakers, &m, capture, movement, notify);
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

            draw_board(&engine, &state, &mut d, &sprite_sheet);
        }
    }
}

fn get_position_form_pixels(mouse_position: Vector2) -> Position {
    let file = mouse_position.x as i32 / PIECE_SIZE;
    let rank = mouse_position.y as i32 / PIECE_SIZE;

    Position(i32::min(file, 7), i32::min(rank, 7))
}
