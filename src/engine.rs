use std::collections::HashSet;

use derive_more::{Add, AddAssign};
use raylib::{
    audio::RaylibAudio,
    ffi::Vector2,
    prelude::{Color, RaylibDraw, RaylibDrawHandle, Rectangle, Sound},
    texture::Texture2D,
    RaylibHandle, RaylibThread,
};

const transparent: Color = Color { r: 255, g: 255, b: 255, a: 100 };
const white: Color = Color { r: 255, g: 255, b: 255, a: 255 };
const black: Color = Color { r: 0, g: 0, b: 0, a: 255 };
const light: Color = Color { r: 235, g: 210, b: 183, a: 255 };
const dark: Color = Color { r: 148, g: 102, b: 83, a: 255 };
const green: Color = Color { r: 130, g: 151, b: 105, a: 180 };
pub const piece_size: i32 = 45;
const starting_position: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; //"1nbqkbnr/pppppppp/8/r3PK2/8/8/PPPP1PPP/RNBQ1BNR w k - 0 1";
const rook_directions: [Position; 4] = [Position(-1, 0), Position(1, 0), Position(0, 1), Position(0, -1)];
const bishop_directions: [Position; 4] = [Position(1, 1), Position(1, -1), Position(-1, 1), Position(-1, -1)];
const knight_jumps: [Position; 8] = [
    Position(1, 2),
    Position(-1, 2),
    Position(1, -2),
    Position(-1, -2),
    Position(2, 1),
    Position(2, -1),
    Position(-2, 1),
    Position(-2, -1),
];
const king_jumps: [Position; 8] = [
    Position(1, 0),
    Position(1, -1),
    Position(0, -1),
    Position(-1, -1),
    Position(-1, 0),
    Position(1, 1),
    Position(0, 1),
    Position(1, 1),
];
const white_pawn_jumps: [Position; 1] = [Position(0, -1)];
const white_pawn_extended_jumps: [Position; 2] = [Position(0, -1), Position(0, -2)];
const white_pawn_attacks: [Position; 2] = [Position(-1, -1), Position(1, -1)];
const black_pawn_jumps: [Position; 1] = [Position(0, 1)];
const black_pawn_extended_jumps: [Position; 2] = [Position(0, 1), Position(0, 2)];
const black_pawn_attacks: [Position; 2] = [Position(-1, 1), Position(1, 1)];
const pawn_enpasant_attacks: [Position; 2] = [Position(-1, 0), Position(1, 0)];

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Team {
    White,
    Black,
}

// TODO give names
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Add, AddAssign)]
pub struct Position(pub i32, pub i32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Piece {
    pub kind: PieceKind,
    pub team: Team,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Move {
    from: Position,
    to: Position,
}

pub enum UndoAction {
    Move(Move, Option<Piece>),
    Enpasant(Move),
    Castle(Move),
    Promotion(Move),
}

pub struct Engine {
    board: [Option<Piece>; 8 * 8],
    history: Vec<Move>,
    pub selected: Option<Position>,
    pub hovered: Position,
    pub moving: bool,
    pub turn: Team,

    sprite_sheet: Texture2D,
    move_sound: Sound,
    capture_sound: Sound,
    speakers: RaylibAudio,
}

impl Engine {
    pub fn new(rl: &mut RaylibHandle, thread: &RaylibThread) -> Self {
        let mut engine = Engine {
            board: [None; 8 * 8],
            history: Vec::new(),
            selected: None,
            hovered: Position(0, 0),
            moving: false,
            turn: Team::White,
            sprite_sheet: rl.load_texture(thread, "assets/Chess_Pieces_Sprite.png").expect("Could not load piece textures"),
            move_sound: Sound::load_sound("assets/move.mp3").expect("Could not load move sound"),
            capture_sound: Sound::load_sound("assets/capture.mp3").expect("Could not load capture sound"),
            speakers: RaylibAudio::init_audio_device(),
        };

        engine.load_position_from_fen(starting_position);

        engine
    }

    fn load_position_from_fen(&mut self, fen: &str) {
        let mut file = 0;
        let mut rank = 7;

        for c in fen.chars() {
            if c == ' ' {
                break;
            } else if c == '/' {
                file = 0;
                rank -= 1;
            } else if let Some(n) = c.to_digit(10) {
                file += n as i32;
            } else {
                let team = if c.is_uppercase() { Team::Black } else { Team::White };
                let kind = match c.to_ascii_lowercase() {
                    'k' => PieceKind::King,
                    'p' => PieceKind::Pawn,
                    'q' => PieceKind::Queen,
                    'r' => PieceKind::Rook,
                    'b' => PieceKind::Bishop,
                    'n' => PieceKind::Knight,
                    _ => unreachable!(),
                };
                *self.get_piece_mut(Position(file, rank)) = Some(Piece { kind, team });
                file += 1;
            }
        }
    }

    pub fn get_piece(&self, Position(file, rank): Position) -> &Option<Piece> {
        &self.board[file as usize + rank as usize * 8]
    }

    fn get_piece_mut(&mut self, Position(file, rank): Position) -> &mut Option<Piece> {
        &mut self.board[file as usize + rank as usize * 8]
    }

    // TODO: Mabe use just a single function
    fn check_king_safety(&self, mut on_defender: impl FnMut(Position), mut on_attacker: impl FnMut(Position)) {
        let king_position = self.get_king_position();

        self.check_sliders(
            king_position,
            &rook_directions,
            |_| {},
            |p| {
                if let Some(piece) = self.get_piece(p) {
                    if piece.team == self.turn {
                        on_defender(p);
                    } else if piece.kind == PieceKind::Rook || piece.kind == PieceKind::Queen {
                        on_attacker(p);
                    }
                }
            },
        );
        self.check_sliders(
            king_position,
            &bishop_directions,
            |_| {},
            |p| {
                if let Some(piece) = self.get_piece(p) {
                    if piece.team == self.turn {
                        on_defender(p);
                    } else if piece.kind == PieceKind::Bishop || piece.kind == PieceKind::Queen {
                        on_attacker(p);
                    }
                }
            },
        );

        self.check_jumpers(
            king_position,
            &knight_jumps,
            |_| {},
            |p| {
                if let Some(piece) = self.get_piece(p) {
                    if piece.team != self.turn && piece.kind == PieceKind::Knight {
                        on_attacker(p);
                    }
                }
            },
        );

        self.check_jumpers(
            king_position,
            &king_jumps,
            |_| {},
            |p| {
                if let Some(piece) = self.get_piece(p) {
                    if piece.team != self.turn && piece.kind == PieceKind::King {
                        on_attacker(p);
                    }
                }
            },
        );

        self.check_jumpers(
            king_position,
            if self.turn == Team::White { &white_pawn_attacks } else { &black_pawn_attacks },
            |_| {},
            |p| {
                if let Some(piece) = self.get_piece(p) {
                    if piece.team != self.turn && piece.kind == PieceKind::Pawn {
                        on_attacker(p);
                    }
                }
            },
        );
    }

    fn get_king_position(&self) -> Position {
        for file in 0..8 {
            for rank in 0..8 {
                if let Some(piece) = self.get_piece(Position(file, rank)).as_ref() {
                    if piece.kind == PieceKind::King && piece.team == self.turn {
                        return Position(file, rank);
                    }
                }
            }
        }
        unreachable!()
    }

    fn check_sliders(&self, origin: Position, directions: &[Position], mut on_empty: impl FnMut(Position), mut on_blocked: impl FnMut(Position)) {
        for &direction in directions {
            let mut current = origin + direction;

            while Engine::in_bounds(current) {
                if self.get_piece(current).is_some() {
                    on_blocked(current);
                    break;
                } else {
                    on_empty(current);
                }
                current += direction;
            }
        }
    }

    fn check_jumpers(&self, origin: Position, ofsets: &[Position], mut on_empty: impl FnMut(Position), mut on_blocked: impl FnMut(Position)) {
        for &offset in ofsets {
            let current = origin + offset;
            if Engine::in_bounds(current) {
                if self.get_piece(current).is_some() {
                    on_blocked(current);
                    break;
                } else {
                    on_empty(current);
                }
            }
        }
    }

    fn in_bounds(Position(file, rank): Position) -> bool {
        file >= 0 && file < 8 && rank >= 0 && rank < 8
    }

    pub fn calculate_valid_moves(&mut self) -> Vec<Move> {
        let mut valid_moves = Vec::new();

        let mut defenders = HashSet::new();
        defenders.insert(self.get_king_position());

        let mut in_check = false;
        self.check_king_safety(
            |_| {},
            |_| {
                in_check = true;
            },
        );

        let is_valid = |m: Move| {
            // !self.uncovers_king(m)
            if in_check || defenders.contains(&m.from) {
                false
            } else {
                true
            }
        };

        for file in 0..8 {
            for rank in 0..8 {
                let piece_position = Position(file, rank);

                if let Some(piece) = self.get_piece(piece_position) {
                    if piece.team != self.turn {
                        continue;
                    }

                    let on_empty = |p: Position| {
                        let m = Move { from: piece_position, to: p };
                        if is_valid(m) {
                            valid_moves.push(m);
                        }
                    };
                    let on_blocked = |p: Position| {
                        let m = Move { from: piece_position, to: p };
                        if let Some(piece) = self.get_piece(p) {
                            if piece.team != self.turn && is_valid(m) {
                                valid_moves.push(m);
                            }
                        }
                    };
                    match piece.kind {
                        PieceKind::Queen => {
                            self.check_sliders(piece_position, &rook_directions, on_empty, on_blocked);
                            self.check_sliders(piece_position, &bishop_directions, on_empty, on_blocked);
                        }
                        PieceKind::Rook => self.check_sliders(piece_position, &rook_directions, on_empty, on_blocked),
                        PieceKind::Bishop => self.check_sliders(piece_position, &bishop_directions, on_empty, on_blocked),
                        PieceKind::Knight => self.check_jumpers(piece_position, &knight_jumps, on_empty, on_blocked),
                        PieceKind::Pawn => {
                            // Moves
                            if self.is_pawn_first_move(piece_position) {
                                self.check_jumpers(
                                    piece_position,
                                    &if piece.team == Team::White { white_pawn_extended_jumps } else { black_pawn_extended_jumps },
                                    on_empty,
                                    |_| {},
                                )
                            } else {
                                self.check_jumpers(piece_position, &if piece.team == Team::White { white_pawn_jumps } else { black_pawn_jumps }, on_empty, |_| {})
                            }
                            // Attack
                            self.check_jumpers(piece_position, &if piece.team == Team::White { white_pawn_attacks } else { black_pawn_attacks }, |_| {}, on_blocked);

                            // En passant
                            let add_if_enpasant = |mut p: Position| {
                                if self.did_enpasant(p) {
                                    let offset = if self.turn == Team::White { Position(0, -1) } else { Position(0, 1) };
                                    p += offset;
                                    if is_valid(Move { from: piece_position, to: p }) {
                                        valid_moves.push(Move { from: piece_position, to: p });
                                    }
                                }
                            };
                            self.check_jumpers(piece_position, &pawn_enpasant_attacks, |_| {}, add_if_enpasant)
                        }
                        PieceKind::King => {}
                    }
                }
            }
        }

        if valid_moves.is_empty() {
            if in_check {
                println!("Checkmate");
            } else {
                println!("Drowned king");
            }
        }

        valid_moves
    }

    fn is_pawn_first_move(&self, Position(_, rank): Position) -> bool {
        (self.turn == Team::White && rank == 6) || (self.turn == Team::Black && rank == 1)
    }

    fn uncovers_king(&mut self, m: Move) -> bool {
        let mut is_invalid = false;

        // Set up new board
        let undo = self.make_move(m);

        // Ckecks the king's safety
        self.check_king_safety(|_| {}, |_| is_invalid = true);

        // Restores the board
        self.undo_move(undo);

        return is_invalid;
    }

    pub fn make_move(&mut self, m: Move) -> UndoAction {
        self.selected = None;

        let original = self.get_piece(m.from).expect("This should never be empty");
        let target = *self.get_piece(m.to);

        if target.is_some() {
            self.speakers.play_sound(&self.capture_sound);
        } else {
            self.speakers.play_sound(&self.move_sound);
        }

        let undo_action = if original.kind == PieceKind::Pawn && m.to.0 != m.from.0 && target.is_none() {
            self.get_piece_mut(Position(m.to.0, m.from.1)).take();
            UndoAction::Enpasant(m)
        } else if original.kind == PieceKind::Pawn && (m.to.1 == 0 || m.to.1 == 7) {
            // TODO Request the user
            self.get_piece_mut(m.from).unwrap().kind = PieceKind::Queen;
            UndoAction::Promotion(m)
        } else if original.kind == PieceKind::King && (m.to.0 - m.from.0).abs() > 1 {
            if (m.to.0 - m.from.0).is_negative() {
                *self.get_piece_mut(Position(3, m.to.1)) = self.get_piece_mut(Position(0, m.to.1)).take();
            } else {
                *self.get_piece_mut(Position(5, m.to.1)) = self.get_piece_mut(Position(7, m.to.1)).take();
            }
            UndoAction::Move(m, target)
        } else {
            UndoAction::Move(m, target)
        };

        *self.get_piece_mut(m.to) = self.get_piece_mut(m.from).take();

        self.toggle_turn();

        undo_action
    }

    pub fn undo_move(&mut self, action: UndoAction) {
        match action {
            UndoAction::Move(m, p) => {
                *self.get_piece_mut(m.from) = self.get_piece_mut(m.to).take();
                *self.get_piece_mut(m.to) = p;
            }
            UndoAction::Enpasant(m) => {
                *self.get_piece_mut(m.from) = self.get_piece_mut(m.to).take();
                *self.get_piece_mut(Position(m.to.0, m.from.1)) = Some(Piece {
                    kind: PieceKind::Pawn,
                    team: self.get_piece(m.from).unwrap().team,
                });
            }
            UndoAction::Castle(m) => {
                *self.get_piece_mut(m.from) = self.get_piece_mut(m.to).take();
                if (m.to.0 - m.from.0).is_negative() {
                    *self.get_piece_mut(Position(0, m.to.1)) = self.get_piece_mut(Position(3, m.to.1)).take();
                } else {
                    *self.get_piece_mut(Position(7, m.to.1)) = self.get_piece_mut(Position(5, m.to.1)).take();
                }
            }
            UndoAction::Promotion(m) => {
                let mut piece = self.get_piece_mut(m.to).take().unwrap();
                piece.kind = PieceKind::Pawn;
                *self.get_piece_mut(m.from) = Some(piece);
            }
        };
        self.toggle_turn();
    }

    fn toggle_turn(&mut self) {
        self.turn = if self.turn == Team::White { Team::Black } else { Team::White };
    }

    fn did_enpasant(&self, p: Position) -> bool {
        let offset = if self.turn == Team::White { Position(0, -2) } else { Position(0, 2) };
        return self.history.last() == Some(&Move { from: p + offset, to: p });
    }

    pub fn draw_board(&self, d: &mut RaylibDrawHandle, valid_moves: &[Move]) {
        self.draw_checker_pattern(d);
        self.draw_pieces(d);
        self.draw_active_piece(d, valid_moves);
    }

    fn draw_checker_pattern(&self, d: &mut RaylibDrawHandle) {
        for file in 0..8 {
            for rank in 0..8 {
                d.draw_rectangle(file * piece_size, rank * piece_size, piece_size, piece_size, if (file + rank) % 2 == 0 { light } else { dark })
            }
        }
    }

    fn draw_pieces(&self, d: &mut RaylibDrawHandle) {
        for file in 0..8 {
            for rank in 0..8 {
                if let Some(piece) = self.get_piece(Position(file, rank)) {
                    let x = piece_size
                        * match piece.kind {
                            PieceKind::Pawn => 5,
                            PieceKind::Rook => 4,
                            PieceKind::Knight => 3,
                            PieceKind::Bishop => 2,
                            PieceKind::Queen => 1,
                            PieceKind::King => 0,
                        };
                    let y = if piece.team == Team::White { 0 } else { piece_size };

                    let color = if self.selected == Some(Position(file, rank)) && self.moving { transparent } else { white };
                    d.draw_texture_rec(
                        &self.sprite_sheet,
                        Rectangle::new(x as f32, y as f32, piece_size as f32, piece_size as f32),
                        Vector2 {
                            x: file as f32 * piece_size as f32,
                            y: rank as f32 * piece_size as f32,
                        },
                        color,
                    );
                }
            }
        }
    }

    fn draw_active_piece(&self, d: &mut RaylibDrawHandle, valid_moves: &[Move]) {
        if let Some(selected) = self.selected {
            for m in valid_moves {
                if m.from == selected {
                    // TODO: If there is a piece to eat dont use a circle, use something else
                    if self.hovered == m.to {
                        d.draw_rectangle(m.to.0 * piece_size, m.to.1 * piece_size, piece_size, piece_size, green);
                    } else {
                        d.draw_circle(m.to.0 * piece_size + (piece_size / 2), m.to.1 * piece_size + (piece_size / 2), (piece_size / 8) as f32, green);
                    }
                }
            }

            if self.moving {
                let mp = d.get_mouse_position();
                let position = Vector2 {
                    x: mp.x - (piece_size / 2) as f32,
                    y: mp.y - (piece_size / 2) as f32,
                };
                let piece = self.get_piece(selected).expect("Fuck");
                // TODO: Extract the match instead of repeating it
                let x = piece_size
                    * match piece.kind {
                        PieceKind::Pawn => 5,
                        PieceKind::Rook => 4,
                        PieceKind::Knight => 3,
                        PieceKind::Bishop => 2,
                        PieceKind::Queen => 1,
                        PieceKind::King => 0,
                    };
                let y = if piece.team == Team::White { 0 } else { piece_size };
                d.draw_texture_rec(&self.sprite_sheet, Rectangle::new(x as f32, y as f32, piece_size as f32, piece_size as f32), position, white);
            }
        }
    }
}
