use std::{collections::HashSet, ops::Mul};

use derive_more::{Add, AddAssign};
use raylib::{
    audio::RaylibAudio,
    ffi::Vector2,
    prelude::{Color, RaylibDraw, RaylibDrawHandle, Rectangle, Sound},
    texture::Texture2D,
    RaylibHandle, RaylibThread,
};

const TRANSPARENT: Color = Color { r: 255, g: 255, b: 255, a: 100 };
const WHITE: Color = Color { r: 255, g: 255, b: 255, a: 255 };
const LIGHT: Color = Color { r: 235, g: 210, b: 183, a: 255 };
const DARK: Color = Color { r: 148, g: 102, b: 83, a: 255 };
const GREEN: Color = Color { r: 130, g: 151, b: 105, a: 180 };
pub const PIECE_SIZE: i32 = 45;
const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const ROOK_DIRECTIONS: [Position; 4] = [Position(-1, 0), Position(1, 0), Position(0, 1), Position(0, -1)];
const BISHOP_DIRECTIONS: [Position; 4] = [Position(1, 1), Position(1, -1), Position(-1, 1), Position(-1, -1)];
const KNIGHT_JUMPS: [Position; 8] = [
    Position(1, 2),
    Position(-1, 2),
    Position(1, -2),
    Position(-1, -2),
    Position(2, 1),
    Position(2, -1),
    Position(-2, 1),
    Position(-2, -1),
];
const KING_JUMPS: [Position; 8] = [
    Position(1, 0),
    Position(1, -1),
    Position(0, -1),
    Position(-1, -1),
    Position(-1, 0),
    Position(-1, 1),
    Position(0, 1),
    Position(1, 1),
];
const WHITE_PAWN_ATTACKS: [Position; 2] = [Position(-1, -1), Position(1, -1)];
const BLACK_PAWN_ATTACKS: [Position; 2] = [Position(-1, 1), Position(1, 1)];

impl Mul<i32> for Position {
    type Output = Position;
    fn mul(self, rhs: i32) -> Self::Output {
        Position(self.0 * rhs, self.1 * rhs)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Team {
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
    pub from: Position,
    pub to: Position,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UndoAction {
    Move(Move, Option<Piece>),
    Enpasant(Move),
    Castle(Move),
    Promotion(Move, Option<Piece>),
}

pub struct Engine {
    pub board: [Option<Piece>; 8 * 8],
    history: Vec<Move>,
    pub selected: Option<Position>,
    pub hovered: Position,
    pub moving: bool,
    pub turn: Team,
    white_king_position: Position,
    black_king_position: Position,

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
            white_king_position: Position(4, 7),
            black_king_position: Position(4, 0),
            sprite_sheet: rl.load_texture(thread, "assets/Chess_Pieces_Sprite.png").expect("Could not load piece textures"),
            speakers: RaylibAudio::init_audio_device(),
            capture_sound: Sound::load_sound("assets/capture.mp3").expect("Could not load capture sound"),
            move_sound: Sound::load_sound("assets/move.mp3").expect("Could not load move sound"),
        };

        engine.load_position_from_fen(STARTING_POSITION);

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

    #[profiling::function]
    fn slider_moves(&self, origin: Position, directions: &[Position], save_move: &mut impl FnMut(Move, &Option<Piece>)) {
        for &direction in directions {
            let mut current = origin + direction;

            while Engine::in_bounds(&current) {
                let piece = self.get_piece(current);
                save_move(Move { from: origin, to: current }, piece);

                if piece.is_some() {
                    break;
                } else {
                    current += direction;
                }
            }
        }
    }

    #[profiling::function]
    fn jumper_moves(&self, origin: Position, ofsets: &[Position], save_move: &mut impl FnMut(Move, &Option<Piece>)) {
        for &offset in ofsets {
            let current = origin + offset;
            if Engine::in_bounds(&current) {
                save_move(Move { from: origin, to: current }, self.get_piece(current));
            }
        }
    }

    #[profiling::function]
    fn pawn_moves(&self, origin: Position, save_move: &mut impl FnMut(Move, &Option<Piece>)) {
        let direction = if self.turn == Team::White { Position(0, -1) } else { Position(0, 1) };

        let move_forward = origin + direction;
        let move_forward_two = move_forward + direction;
        let move_left = origin + Position(-1, 0);
        let move_right = origin + Position(1, 0);
        let attack_left = move_forward + Position(-1, 0);
        let attack_right = move_forward + Position(1, 0);

        if self.get_piece(move_forward).is_none() {
            save_move(Move { from: origin, to: move_forward }, &None);
            if self.is_pawn_first_move(origin) && self.get_piece(move_forward_two).is_none() {
                let m = Move { from: origin, to: move_forward_two };
                save_move(m, &None);
            }
        }

        if Engine::in_bounds(&attack_left) {
            if let Some(piece) = self.get_piece(attack_left) {
                if piece.team != self.turn {
                    let m = Move { from: origin, to: attack_left };
                    save_move(m, &None);
                }
            } else if self.pawn_did_double_move(move_left) {
                let m = Move { from: origin, to: attack_left };
                save_move(m, &None);
            }
        }

        if Engine::in_bounds(&attack_right) {
            if let Some(piece) = self.get_piece(attack_right) {
                if piece.team != self.turn {
                    let m = Move { from: origin, to: attack_right };
                    save_move(m, &None);
                }
            } else if self.pawn_did_double_move(move_right) {
                let m = Move { from: origin, to: attack_right };
                save_move(m, &None);
            }
        }
    }

    #[profiling::function]
    fn is_king_safe(&self, mut save_defender: impl FnMut(Position)) -> bool {
        let mut safe = true;

        let position = if self.turn == Team::White { self.white_king_position } else { self.black_king_position };

        // Check rooks and queens
        self.slider_moves(position, &ROOK_DIRECTIONS, &mut |m: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team == self.turn => save_defender(m.to),
            Some(piece) if piece.team != self.turn && (piece.kind == PieceKind::Rook || piece.kind == PieceKind::Queen) => safe = false,
            _ => (),
        });

        // Check bishops and queens
        self.slider_moves(position, &BISHOP_DIRECTIONS, &mut |m: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team == self.turn => save_defender(m.to),
            Some(piece) if piece.team != self.turn && (piece.kind == PieceKind::Bishop || piece.kind == PieceKind::Queen) => safe = false,
            _ => (),
        });

        // Check knights
        self.jumper_moves(position, &KNIGHT_JUMPS, &mut |_: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team != self.turn && piece.kind == PieceKind::Knight => safe = false,
            _ => (),
        });

        // Check enemy king
        self.jumper_moves(position, &KING_JUMPS, &mut |_: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team != self.turn && piece.kind == PieceKind::King => safe = false,
            _ => (),
        });

        // Check pawns
        let moves = &if self.turn == Team::White { WHITE_PAWN_ATTACKS } else { BLACK_PAWN_ATTACKS };
        self.jumper_moves(position, moves, &mut |_: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team != self.turn && piece.kind == PieceKind::Pawn => safe = false,
            _ => (),
        });

        safe
    }

    #[profiling::function]
    pub fn calculate_valid_moves(&mut self) -> Vec<Move> {
        let mut valid_moves = Vec::new();

        let mut defenders = HashSet::new();
        defenders.insert(if self.turn == Team::White { self.white_king_position } else { self.black_king_position });

        let in_check = !self.is_king_safe(|p| {
            defenders.insert(p);
        });

        let turn = self.turn;
        let mut save_move = |m: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team == turn => (),
            _ => valid_moves.push(m),
        };

        for file in 0..8 {
            for rank in 0..8 {
                let piece_position = Position(file, rank);

                if let Some(piece) = self.get_piece(piece_position) {
                    if piece.team != self.turn {
                        continue;
                    }

                    match piece.kind {
                        PieceKind::Queen => {
                            self.slider_moves(piece_position, &ROOK_DIRECTIONS, &mut save_move);
                            self.slider_moves(piece_position, &BISHOP_DIRECTIONS, &mut save_move);
                        }
                        PieceKind::Rook => self.slider_moves(piece_position, &ROOK_DIRECTIONS, &mut save_move),
                        PieceKind::Bishop => self.slider_moves(piece_position, &BISHOP_DIRECTIONS, &mut save_move),
                        PieceKind::Knight => self.jumper_moves(piece_position, &KNIGHT_JUMPS, &mut save_move),
                        PieceKind::Pawn => self.pawn_moves(piece_position, &mut save_move),
                        PieceKind::King => {
                            self.jumper_moves(piece_position, &KING_JUMPS, &mut save_move);
                            let left1 = piece_position + Position(-1, 0);
                            let left2 = piece_position + Position(-2, 0);
                            let left3 = piece_position + Position(-3, 0);
                            let right1 = piece_position + Position(1, 0);
                            let right2 = piece_position + Position(2, 0);
                            let left_rook = Position(0, piece_position.1);
                            let right_rook = Position(7, piece_position.1);
                            let king_position = Position(4, if self.turn == Team::White { 7 } else { 0 });

                            if !in_check
                                && Engine::in_bounds(&right1)
                                && self.get_piece(right1).is_none()
                                && Engine::in_bounds(&right2)
                                && self.get_piece(right2).is_none()
                                && !self.history.iter().any(|m| m.from == right_rook || m.to == king_position)
                                && !self.uncovers_king(Move { from: piece_position, to: right1 })
                                && !self.uncovers_king(Move { from: piece_position, to: right2 })
                            {
                                save_move(Move { from: piece_position, to: right2 }, &None);
                            }
                            if !in_check
                                && Engine::in_bounds(&left1)
                                && self.get_piece(left1).is_none()
                                && Engine::in_bounds(&left2)
                                && self.get_piece(left2).is_none()
                                && Engine::in_bounds(&left3)
                                && self.get_piece(left3).is_none()
                                && !self.history.iter().any(|m| m.from == left_rook || m.to == king_position)
                                && !self.uncovers_king(Move { from: piece_position, to: left1 })
                                && !self.uncovers_king(Move { from: piece_position, to: left2 })
                            {
                                save_move(Move { from: piece_position, to: left2 }, &None);
                            }
                        }
                    }
                }
            }
        }

        let needs_checking = |m: Move| in_check || defenders.contains(&m.from);
        valid_moves.retain(|&m| !needs_checking(m) || !self.uncovers_king(m));

        if valid_moves.is_empty() {
            if in_check {
                println!("Checkmate");
            } else {
                println!("Drowned king");
            }
        }

        valid_moves
    }

    fn in_bounds(Position(file, rank): &Position) -> bool {
        (0..8).contains(file) && (0..8).contains(rank)
    }

    fn is_pawn_first_move(&self, Position(_, rank): Position) -> bool {
        (self.turn == Team::White && rank == 6) || (self.turn == Team::Black && rank == 1)
    }

    fn uncovers_king(&mut self, m: Move) -> bool {
        // Set up new board, order is important
        let undo = self.make_move(m);
        self.toggle_turn();

        // Ckecks the king's safety
        let is_invalid = !self.is_king_safe(|_| {});

        // Restores the board order is important
        self.undo_move(undo);
        self.toggle_turn();

        is_invalid
    }

    #[profiling::function]
    pub fn make_move(&mut self, m: Move) -> UndoAction {
        self.selected = None;

        let original = self.get_piece(m.from).expect("This should never be empty");
        let target = *self.get_piece(m.to);

        if m.from == self.white_king_position {
            self.white_king_position = m.to;
        } else if m.from == self.black_king_position {
            self.black_king_position = m.to;
        }

        if target.is_some() {
            self.speakers.play_sound(&self.capture_sound);
        } else {
            self.speakers.play_sound(&self.move_sound);
        }
        self.toggle_turn();
        self.history.push(m);

        let undo_action = if original.kind == PieceKind::Pawn && m.to.0 != m.from.0 && target.is_none() {
            self.get_piece_mut(Position(m.to.0, m.from.1)).take();
            UndoAction::Enpasant(m)
        } else if original.kind == PieceKind::Pawn && (m.to.1 == 0 || m.to.1 == 7) {
            // TODO Request the user
            self.get_piece_mut(m.from).as_mut().unwrap().kind = PieceKind::Queen;
            UndoAction::Promotion(m, target)
        } else if original.kind == PieceKind::King && (m.to.0 - m.from.0).abs() > 1 {
            if (m.to.0 - m.from.0).is_negative() {
                *self.get_piece_mut(Position(3, m.to.1)) = self.get_piece_mut(Position(0, m.to.1)).take();
            } else {
                *self.get_piece_mut(Position(5, m.to.1)) = self.get_piece_mut(Position(7, m.to.1)).take();
            }
            UndoAction::Castle(m)
        } else {
            UndoAction::Move(m, target)
        };

        *self.get_piece_mut(m.to) = self.get_piece_mut(m.from).take();

        undo_action
    }

    #[profiling::function]
    pub fn undo_move(&mut self, action: UndoAction) {
        match action {
            UndoAction::Move(m, p) => {
                *self.get_piece_mut(m.from) = self.get_piece_mut(m.to).take();
                *self.get_piece_mut(m.to) = p;

                if m.to == self.white_king_position {
                    self.white_king_position = m.from;
                } else if m.to == self.black_king_position {
                    self.black_king_position = m.from;
                }
            }
            UndoAction::Enpasant(m) => {
                *self.get_piece_mut(m.from) = self.get_piece_mut(m.to).take();
                *self.get_piece_mut(Position(m.to.0, m.from.1)) = Some(Piece {
                    kind: PieceKind::Pawn,
                    team: if self.get_piece(m.from).unwrap().team == Team::White { Team::Black } else { Team::White },
                });
            }
            UndoAction::Castle(m) => {
                *self.get_piece_mut(m.from) = self.get_piece_mut(m.to).take();
                if (m.to.0 - m.from.0).is_negative() {
                    *self.get_piece_mut(Position(0, m.to.1)) = self.get_piece_mut(Position(3, m.to.1)).take();
                } else {
                    *self.get_piece_mut(Position(7, m.to.1)) = self.get_piece_mut(Position(5, m.to.1)).take();
                }

                if m.to == self.white_king_position {
                    self.white_king_position = m.from;
                } else if m.to == self.black_king_position {
                    self.black_king_position = m.from;
                }
            }
            UndoAction::Promotion(m, p) => {
                let mut piece = self.get_piece_mut(m.to).take().unwrap();
                piece.kind = PieceKind::Pawn;
                *self.get_piece_mut(m.from) = Some(piece);
                *self.get_piece_mut(m.to) = p;
            }
        };

        // TODO Play undo sound
        self.toggle_turn();
        self.history.pop();
    }

    fn toggle_turn(&mut self) {
        self.turn = if self.turn == Team::White { Team::Black } else { Team::White };
    }

    fn pawn_did_double_move(&self, p: Position) -> bool {
        let offset = if self.turn == Team::White { Position(0, -2) } else { Position(0, 2) };
        match self.get_piece(p) {
            Some(piece) => piece.kind == PieceKind::Pawn && self.history.last() == Some(&Move { from: p + offset, to: p }),
            _ => false,
        }
    }

    pub fn draw_board(&self, d: &mut RaylibDrawHandle, valid_moves: &[Move]) {
        self.draw_checker_pattern(d);
        self.draw_pieces(d);
        self.draw_active_piece(d, valid_moves);
    }

    fn draw_checker_pattern(&self, d: &mut RaylibDrawHandle) {
        for file in 0..8 {
            for rank in 0..8 {
                d.draw_rectangle(file * PIECE_SIZE, rank * PIECE_SIZE, PIECE_SIZE, PIECE_SIZE, if (file + rank) % 2 == 0 { LIGHT } else { DARK })
            }
        }
    }

    fn draw_pieces(&self, d: &mut RaylibDrawHandle) {
        for file in 0..8 {
            for rank in 0..8 {
                if let Some(piece) = self.get_piece(Position(file, rank)) {
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

                    let color = if self.selected == Some(Position(file, rank)) && self.moving { TRANSPARENT } else { WHITE };
                    d.draw_texture_rec(
                        &self.sprite_sheet,
                        Rectangle::new(x as f32, y as f32, PIECE_SIZE as f32, PIECE_SIZE as f32),
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

    fn draw_active_piece(&self, d: &mut RaylibDrawHandle, valid_moves: &[Move]) {
        if let Some(selected) = self.selected {
            for m in valid_moves {
                if m.from == selected {
                    // TODO: If there is a piece to eat dont use a circle, use something else
                    if self.hovered == m.to {
                        d.draw_rectangle(m.to.0 * PIECE_SIZE, m.to.1 * PIECE_SIZE, PIECE_SIZE, PIECE_SIZE, GREEN);
                    } else {
                        d.draw_circle(m.to.0 * PIECE_SIZE + (PIECE_SIZE / 2), m.to.1 * PIECE_SIZE + (PIECE_SIZE / 2), (PIECE_SIZE / 8) as f32, GREEN);
                    }
                }
            }

            if self.moving {
                let mp = d.get_mouse_position();
                let position = Vector2 {
                    x: mp.x - (PIECE_SIZE / 2) as f32,
                    y: mp.y - (PIECE_SIZE / 2) as f32,
                };
                let piece = self.get_piece(selected).expect("Fuck");
                // TODO: Extract the match instead of repeating it
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
                d.draw_texture_rec(&self.sprite_sheet, Rectangle::new(x as f32, y as f32, PIECE_SIZE as f32, PIECE_SIZE as f32), position, WHITE);
            }
        }
    }
}
