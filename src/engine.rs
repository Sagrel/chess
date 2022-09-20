use std::{collections::HashSet, ops::Mul};

use derive_more::{Add, AddAssign};
use raylib::{
    ffi::Vector2,
    prelude::{Color, RaylibDraw, RaylibDrawHandle, Rectangle},
    texture::Texture2D,
};

use crate::{GameState, SoundType};

const TRANSPARENT: Color = Color { r: 255, g: 255, b: 255, a: 100 };
const WHITE: Color = Color { r: 255, g: 255, b: 255, a: 255 };
const LIGHT: Color = Color { r: 235, g: 210, b: 183, a: 255 };
const DARK: Color = Color { r: 148, g: 102, b: 83, a: 255 };
const GREEN: Color = Color { r: 130, g: 151, b: 105, a: 180 };
pub const PIECE_SIZE: i32 = 45;
pub const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
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
    pub kind: MoveKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MoveKind {
    Normal,
    Double,
    Casttle,
    Enpasant,
    Promote(PieceKind),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct State {
    can_cattle_left_white: bool,
    can_cattle_right_white: bool,
    can_cattle_left_black: bool,
    can_cattle_right_black: bool,
    white_king_position: Position,
    black_king_position: Position,
    did_double_move: Option<Position>,
    half_moves: usize, // TODO implement half moves
    full_moves: usize, // TODO implement full moves
    pub turn: Team,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UndoKind {
    Move(Option<Piece>),
    Enpasant,
    Castle,
    Promotion(Option<Piece>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndoAction {
    movement: Move,
    state: State,
    kind: UndoKind,
}

pub struct Engine {
    pub board: [Option<Piece>; 8 * 8],
    pub state: State,
}

impl Engine {
    pub fn new(fen: &str) -> Self {
        let mut engine = Engine {
            board: [None; 8 * 8],
            state: State {
                half_moves: 0,
                full_moves: 0,
                turn: Team::White,
                can_cattle_left_white: true,
                can_cattle_right_white: true,
                can_cattle_left_black: true,
                can_cattle_right_black: true,
                did_double_move: None,
                white_king_position: Position(4, 7),
                black_king_position: Position(4, 0),
            },
        };

        engine.load_position_from_fen(fen);

        engine
    }

    pub fn load_position_from_fen(&mut self, fen: &str) {
        let mut file = 0;
        let mut rank = 0;

        let mut fields = fen.split_ascii_whitespace();

        // Set piece positions
        for c in fields.next().unwrap().chars() {
            if c == '/' {
                file = 0;
                rank += 1;
            } else if let Some(n) = c.to_digit(10) {
                file += n as i32;
            } else {
                let team = if c.is_uppercase() { Team::White } else { Team::Black };
                let kind = match c.to_ascii_lowercase() {
                    'k' => PieceKind::King,
                    'p' => PieceKind::Pawn,
                    'q' => PieceKind::Queen,
                    'r' => PieceKind::Rook,
                    'b' => PieceKind::Bishop,
                    'n' => PieceKind::Knight,
                    _ => unreachable!(),
                };
                let position = Position(file, rank);
                if team == Team::White && kind == PieceKind::King {
                    self.state.white_king_position = position
                } else if team == Team::Black && kind == PieceKind::King {
                    self.state.black_king_position = position
                }
                *self.get_piece_mut(position) = Some(Piece { kind, team });
                file += 1;
            }
        }

        // Set turn
        match fields.next() {
            Some("w") => self.state.turn = Team::White,
            Some("b") => self.state.turn = Team::Black,
            _ => unreachable!(),
        }

        // Set casttling rights
        for c in fields.next().unwrap().chars() {
            match c {
                'K' => self.state.can_cattle_right_black = true,
                'Q' => self.state.can_cattle_left_black = true,
                'k' => self.state.can_cattle_right_white = true,
                'q' => self.state.can_cattle_left_white = true,
                '-' => {
                    self.state.can_cattle_right_black = false;
                    self.state.can_cattle_left_black = false;
                    self.state.can_cattle_right_white = false;
                    self.state.can_cattle_left_white = false;
                }
                _ => unreachable!(),
            }
        }

        self.state.did_double_move = match fields.next() {
            Some("-") => None,
            Some(x) => Some(Engine::algebraic_notation_to_position(x)),
            _ => unreachable!(),
        };

        self.state.half_moves = fields.next().unwrap().parse().unwrap();

        self.state.full_moves = fields.next().unwrap().parse().unwrap();
    }

    // TODO Add unit tests for this
    fn algebraic_notation_to_position(p: &str) -> Position {
        let mut chars = p.chars();
        Position(chars.next().unwrap() as i32 - 'a' as i32, chars.next().unwrap().to_digit(10).unwrap() as i32)
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
                save_move(
                    Move {
                        from: origin,
                        to: current,
                        kind: MoveKind::Normal,
                    },
                    piece,
                );

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
                save_move(
                    Move {
                        from: origin,
                        to: current,
                        kind: MoveKind::Normal,
                    },
                    self.get_piece(current),
                );
            }
        }
    }

    #[profiling::function]
    fn pawn_moves(&self, origin: Position, save_move: &mut impl FnMut(Move, &Option<Piece>)) {
        let direction = if self.state.turn == Team::White { Position(0, -1) } else { Position(0, 1) };

        let move_forward = origin + direction;
        let move_forward_two = move_forward + direction;
        let move_left = origin + Position(-1, 0);
        let move_right = origin + Position(1, 0);
        let attack_left = move_forward + Position(-1, 0);
        let attack_right = move_forward + Position(1, 0);

        let mut check_promotion = |target: Position, kind: MoveKind| {
            if target.1 == 0 || target.1 == 7 {
                save_move(
                    Move {
                        from: origin,
                        to: target,
                        kind: MoveKind::Promote(PieceKind::Bishop),
                    },
                    &None,
                );
                save_move(
                    Move {
                        from: origin,
                        to: target,
                        kind: MoveKind::Promote(PieceKind::Rook),
                    },
                    &None,
                );
                save_move(
                    Move {
                        from: origin,
                        to: target,
                        kind: MoveKind::Promote(PieceKind::Queen),
                    },
                    &None,
                );
                save_move(
                    Move {
                        from: origin,
                        to: target,
                        kind: MoveKind::Promote(PieceKind::Knight),
                    },
                    &None,
                );
            } else {
                save_move(Move { from: origin, to: target, kind }, &None);
            }
        };

        if self.get_piece(move_forward).is_none() {
            check_promotion(move_forward, MoveKind::Normal);
            if self.is_pawn_first_move(origin) && self.get_piece(move_forward_two).is_none() {
                check_promotion(move_forward_two, MoveKind::Double);
            }
        }

        if Engine::in_bounds(&attack_left) {
            if let Some(piece) = self.get_piece(attack_left) {
                if piece.team != self.state.turn {
                    check_promotion(attack_left, MoveKind::Normal);
                }
            } else if self.pawn_did_double_move(move_left) {
                check_promotion(attack_left, MoveKind::Enpasant);
            }
        }

        if Engine::in_bounds(&attack_right) {
            if let Some(piece) = self.get_piece(attack_right) {
                if piece.team != self.state.turn {
                    check_promotion(attack_right, MoveKind::Normal);
                }
            } else if self.pawn_did_double_move(move_right) {
                check_promotion(attack_right, MoveKind::Enpasant);
            }
        }
    }

    fn get_king_position(&self) -> Position {
        if self.state.turn == Team::White {
            self.state.white_king_position
        } else {
            self.state.black_king_position
        }
    }

    // TODO SPEED Save only the defender pieces if they are actually protecting from an attack
    // TODO SPEED Maybe have 2 function, one only checks if we are under attack and the other calculates what pieces are attacking us and where we need blockers
    #[profiling::function]
    fn is_position_safe(&self, position: Position, mut save_defender: impl FnMut(Position)) -> bool {
        let mut safe = true;

        // Check rooks and queens
        self.slider_moves(position, &ROOK_DIRECTIONS, &mut |m: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team == self.state.turn => save_defender(m.to),
            Some(piece) if piece.team != self.state.turn && (piece.kind == PieceKind::Rook || piece.kind == PieceKind::Queen) => safe = false,
            _ => (),
        });

        // Check bishops and queens
        self.slider_moves(position, &BISHOP_DIRECTIONS, &mut |m: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team == self.state.turn => save_defender(m.to),
            Some(piece) if piece.team != self.state.turn && (piece.kind == PieceKind::Bishop || piece.kind == PieceKind::Queen) => safe = false,
            _ => (),
        });

        // Check knights
        self.jumper_moves(position, &KNIGHT_JUMPS, &mut |_: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team != self.state.turn && piece.kind == PieceKind::Knight => safe = false,
            _ => (),
        });

        // Check enemy king
        self.jumper_moves(position, &KING_JUMPS, &mut |_: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team != self.state.turn && piece.kind == PieceKind::King => safe = false,
            _ => (),
        });

        // Check pawns
        let moves = &if self.state.turn == Team::White { WHITE_PAWN_ATTACKS } else { BLACK_PAWN_ATTACKS };
        self.jumper_moves(position, moves, &mut |_: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team != self.state.turn && piece.kind == PieceKind::Pawn => safe = false,
            _ => (),
        });

        safe
    }

    #[profiling::function]
    pub fn calculate_valid_moves(&mut self) -> Vec<Move> {
        let mut valid_moves = Vec::new();

        let mut defenders = HashSet::new();
        defenders.insert(if self.state.turn == Team::White {
            self.state.white_king_position
        } else {
            self.state.black_king_position
        });

        let in_check = !self.is_position_safe(self.get_king_position(), |p| {
            defenders.insert(p);
        });

        let turn = self.state.turn;
        let mut save_move = |m: Move, piece: &Option<Piece>| match piece {
            Some(piece) if piece.team == turn => (),
            _ => valid_moves.push(m),
        };

        for file in 0..8 {
            for rank in 0..8 {
                let piece_position = Position(file, rank);

                if let Some(piece) = self.get_piece(piece_position) {
                    if piece.team != self.state.turn {
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

                            if !in_check
                                && if self.state.turn == Team::White {
                                    self.state.can_cattle_right_white
                                } else {
                                    self.state.can_cattle_right_black
                                }
                                && self.get_piece(right1).is_none()
                                && self.get_piece(right2).is_none()
                                && self.is_position_safe(right1, |_| {})
                                && self.is_position_safe(right2, |_| {})
                            {
                                save_move(
                                    Move {
                                        from: piece_position,
                                        to: right2,
                                        kind: MoveKind::Casttle,
                                    },
                                    &None,
                                );
                            }
                            if !in_check
                                && if self.state.turn == Team::White {
                                    self.state.can_cattle_left_white
                                } else {
                                    self.state.can_cattle_left_black
                                }
                                && self.get_piece(left1).is_none()
                                && self.get_piece(left2).is_none()
                                && self.get_piece(left3).is_none()
                                && self.is_position_safe(left1, |_| {})
                                && self.is_position_safe(left2, |_| {})
                            {
                                save_move(
                                    Move {
                                        from: piece_position,
                                        to: left2,
                                        kind: MoveKind::Casttle,
                                    },
                                    &None,
                                );
                            }
                        }
                    }
                }
            }
        }

        // TODO SPEED If in check, remove only the moves that do not kill the attacker or block their line of attack
        let needs_checking = |m: Move| in_check || defenders.contains(&m.from);
        valid_moves.retain(|&m| !needs_checking(m) || !self.uncovers_king(m));

        // TODO take a lambda to handle the UI changes and playing a sound
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
        (self.state.turn == Team::White && rank == 6) || (self.state.turn == Team::Black && rank == 1)
    }

    fn uncovers_king(&mut self, m: Move) -> bool {
        // TODO SPEED remove the copy of the board, it's only used to debug
        let board = self.board;
        // Set up new board
        let undo = self.make_move(m, &mut |_| {});
        self.toggle_turn();

        // Ckecks the king's safety
        let is_invalid = !self.is_position_safe(self.get_king_position(), |_| {});

        // Restores the board
        self.undo_move(undo);
        self.toggle_turn();

        if board != self.board {
            println!("{undo:?}")
        }
        is_invalid
    }

    #[profiling::function]
    pub fn make_move(&mut self, m: Move, play_sound: &mut impl FnMut(SoundType)) -> UndoAction {
        let target = *self.get_piece(m.to);

        let old_state = self.state;

        self.state.did_double_move = None;
        if m.from == self.state.white_king_position {
            self.state.white_king_position = m.to;
            self.state.can_cattle_right_white = false;
            self.state.can_cattle_left_white = false;
        } else if m.from == self.state.black_king_position {
            self.state.black_king_position = m.to;
            self.state.can_cattle_right_black = false;
            self.state.can_cattle_left_black = false;
        } else if m.from == Position(0, 0) {
            self.state.can_cattle_left_black = false;
        } else if m.from == Position(7, 0) {
            self.state.can_cattle_right_black = false;
        } else if m.from == Position(0, 7) {
            self.state.can_cattle_left_white = false;
        } else if m.from == Position(7, 7) {
            self.state.can_cattle_right_white = false;
        }

        self.toggle_turn();

        let undo_kind = match m.kind {
            MoveKind::Normal => {
                if target.is_some() {
                    play_sound(SoundType::Capture);
                } else {
                    play_sound(SoundType::Move);
                }

                UndoKind::Move(target)
            }
            MoveKind::Double => {
                self.state.did_double_move = Some(m.to);

                play_sound(SoundType::Move);

                UndoKind::Move(target)
            }
            MoveKind::Casttle => {
                if (m.to.0 - m.from.0).is_negative() {
                    *self.get_piece_mut(Position(3, m.to.1)) = self.get_piece_mut(Position(0, m.to.1)).take();
                } else {
                    *self.get_piece_mut(Position(5, m.to.1)) = self.get_piece_mut(Position(7, m.to.1)).take();
                }

                play_sound(SoundType::Move);

                UndoKind::Castle
            }
            MoveKind::Enpasant => {
                self.get_piece_mut(Position(m.to.0, m.from.1)).take();

                play_sound(SoundType::Capture);

                UndoKind::Enpasant
            }
            MoveKind::Promote(kind) => {
                self.get_piece_mut(m.from).as_mut().unwrap().kind = kind;

                play_sound(SoundType::Move);
                UndoKind::Promotion(target)
            }
        };

        *self.get_piece_mut(m.to) = self.get_piece_mut(m.from).take();

        UndoAction {
            movement: m,
            state: old_state,
            kind: undo_kind,
        }
    }

    // TODO Play undo sound if wanted
    #[profiling::function]
    pub fn undo_move(&mut self, action: UndoAction) {
        *self.get_piece_mut(action.movement.from) = self.get_piece_mut(action.movement.to).take();
        match action.kind {
            UndoKind::Move(p) => {
                *self.get_piece_mut(action.movement.to) = p;
            }
            UndoKind::Enpasant => {
                *self.get_piece_mut(Position(action.movement.to.0, action.movement.from.1)) = Some(Piece {
                    kind: PieceKind::Pawn,
                    team: if action.state.turn == Team::White { Team::Black } else { Team::White },
                });
            }
            UndoKind::Castle => {
                if (action.movement.to.0 - action.movement.from.0).is_negative() {
                    *self.get_piece_mut(Position(0, action.movement.to.1)) = self.get_piece_mut(Position(3, action.movement.to.1)).take();
                } else {
                    *self.get_piece_mut(Position(7, action.movement.to.1)) = self.get_piece_mut(Position(5, action.movement.to.1)).take();
                }
            }
            UndoKind::Promotion(p) => {
                self.get_piece_mut(action.movement.from).as_mut().unwrap().kind = PieceKind::Pawn;
                *self.get_piece_mut(action.movement.to) = p;
            }
        };

        self.state = action.state;
    }

    fn toggle_turn(&mut self) {
        self.state.turn = if self.state.turn == Team::White { Team::Black } else { Team::White };
    }

    fn pawn_did_double_move(&self, p: Position) -> bool {
        self.state.did_double_move == Some(p)
    }

    pub fn draw_board(&self, state: &GameState, d: &mut RaylibDrawHandle, sprite_sheet: &Texture2D) {
        self.draw_checker_pattern(d);
        self.draw_pieces(state, d, sprite_sheet);
        self.draw_active_piece(state, d, sprite_sheet);
    }

    fn draw_checker_pattern(&self, d: &mut RaylibDrawHandle) {
        for file in 0..8 {
            for rank in 0..8 {
                d.draw_rectangle(file * PIECE_SIZE, rank * PIECE_SIZE, PIECE_SIZE, PIECE_SIZE, if (file + rank) % 2 == 0 { LIGHT } else { DARK })
            }
        }
    }

    fn draw_pieces(&self, state: &GameState, d: &mut RaylibDrawHandle, sprite_sheet: &Texture2D) {
        for file in 0..8 {
            for rank in 0..8 {
                if let Some(piece) = self.get_piece(Position(file, rank)) {
                    let (x, y) = Engine::sprite_ofset(piece);

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

    fn draw_active_piece(&self, state: &GameState, d: &mut RaylibDrawHandle, sprite_sheet: &Texture2D) {
        if let Some(selected) = state.selected {
            for m in &state.valid_moves {
                if m.from == selected {
                    if state.hovered == m.to {
                        d.draw_rectangle(m.to.0 * PIECE_SIZE, m.to.1 * PIECE_SIZE, PIECE_SIZE, PIECE_SIZE, GREEN);
                    } else if self.get_piece(m.to).is_some() {
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

                let (x, y) = Engine::sprite_ofset(self.get_piece(selected).as_ref().unwrap());
                d.draw_texture_rec(sprite_sheet, Rectangle::new(x, y, PIECE_SIZE as f32, PIECE_SIZE as f32), position, WHITE);
            }
        }
    }

    pub fn sprite_ofset(piece: &Piece) -> (f32, f32) {
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
}
