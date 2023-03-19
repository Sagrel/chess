use std::{collections::HashSet, ops::Mul};
mod test;
use derive_more::{Add, AddAssign};
use serde::{Deserialize, Serialize};

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn other(self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

// TODO give names
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Add, AddAssign, Serialize, Deserialize)]
pub struct Position(pub i32, pub i32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Piece {
    pub kind: PieceKind,
    pub color: Color,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Move {
    pub from: Position,
    pub to: Position,
    pub kind: MoveKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Undo {
    pub old_state: State,
    pub captured: Option<Piece>,
    pub movement: Move,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Side {
    King,
    Queen,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum MoveKind {
    Normal,
    Double,
    Castle(Side),
    EnPassant,
    Promote(PieceKind),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Outcome {
    Checkmate,
    Drowned,
    Continue(Vec<Move>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct State {
    can_castle_queen_side_white: bool,
    can_castle_king_side_white: bool,
    can_castle_queen_side_black: bool,
    can_castle_king_side_black: bool,
    white_king_position: Position,
    black_king_position: Position,
    did_double_move: Option<Position>,
    half_moves: usize, // TODO implement half moves
    full_moves: usize, // TODO implement full moves
    pub turn: Color,
}


pub struct Engine {
    pub board: [Option<Piece>; 8 * 8],
    pub state: State,
}

/// Removes the elements in `v` for which `f` returns false and returns that modified vector
fn clean<T>(mut v: Vec<T>, mut f: impl FnMut(&T) -> bool) -> Vec<T> {
    let mut idx = 0;
    while idx < v.len() {
        if f(&v[idx]) {
            v.swap_remove(idx);
        } else {
            idx += 1;
        }
    }
    v
}

impl Engine {
    pub fn new(fen: &str) -> Self {
        let mut engine = Engine {
            board: [None; 8 * 8],
            state: State {
                half_moves: 0,
                full_moves: 0,
                turn: Color::White,
                can_castle_queen_side_white: true,
                can_castle_king_side_white: true,
                can_castle_queen_side_black: true,
                can_castle_king_side_black: true,
                did_double_move: None,
                white_king_position: Position(4, 7),
                black_king_position: Position(4, 0),
            },
        };

        engine.load_from_fen(fen);

        engine
    }

    /// Serializes the board and state into a `fen` string
    pub fn to_fen(&self) -> String {
        let mut res = "".to_string();
        let mut count = 0;
        for rank in 0..8 {
            for file in 0..8 {
                let piece = self.get_piece(Position(file, rank));

                if let Some(piece) = piece {
                    if count != 0 {
                        res += &count.to_string();
                        count = 0;
                    }
                    let letter = match piece.kind {
                        PieceKind::Pawn => "p",
                        PieceKind::Rook => "r",
                        PieceKind::Knight => "n",
                        PieceKind::Bishop => "b",
                        PieceKind::Queen => "q",
                        PieceKind::King => "k",
                    };
                    res += &if piece.color == Color::White { letter.to_uppercase() } else { letter.to_lowercase() }
                // TODO make this a method of Color
                } else {
                    count += 1;
                }
            }
            if count != 0 {
                res += &count.to_string();
                count = 0;
            }
            res += "/"
        }

        res.pop();

        res += " ";
        res += if self.state.turn == Color::White { "w" } else { "b" }; // TODO make this a method of Color
        res += " ";

        let size = res.len();

        if self.state.can_castle_queen_side_black {
            res += "q";
        }
        if self.state.can_castle_king_side_black {
            res += "k";
        }
        if self.state.can_castle_queen_side_white {
            res += "Q";
        }
        if self.state.can_castle_king_side_white {
            res += "K";
        }

        if res.len() == size {
            res += "-"
        }

        res += " ";

        if let Some(p) = self.state.did_double_move {
            res += &Engine::position_to_algebraic_notation(p);
        } else {
            res += "-"
        }

        res += " ";

        res += &self.state.half_moves.to_string();

        res += " ";

        res += &self.state.full_moves.to_string();

        res
    }

    /// Sets up the board and the state from a `fen` string
    /// TODO: Should this just return an `Engine` instead of mutating one?
    pub fn load_from_fen(&mut self, fen: &str) {
        let mut fields = fen.split_ascii_whitespace();

        // 1. Set piece positions
        let mut file = 0;
        let mut rank = 0;
        for c in fields.next().unwrap().chars() {
            if c == '/' {
                file = 0;
                rank += 1;
            } else if let Some(n) = c.to_digit(10) {
                file += n as i32;
            } else {
                let team = if c.is_uppercase() { Color::White } else { Color::Black };
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
                if team == Color::White && kind == PieceKind::King {
                    self.state.white_king_position = position
                } else if team == Color::Black && kind == PieceKind::King {
                    self.state.black_king_position = position
                }
                *self.get_piece_mut(position) = Some(Piece { kind, color: team });
                file += 1;
            }
        }

        // 2. Set turn
        match fields.next() {
            Some("w") => self.state.turn = Color::White,
            Some("b") => self.state.turn = Color::Black,
            _ => unreachable!(),
        }

        // 3. Set castling rights
        for c in fields.next().unwrap().chars() {
            match c {
                'K' => self.state.can_castle_king_side_black = true,
                'Q' => self.state.can_castle_queen_side_black = true,
                'k' => self.state.can_castle_king_side_white = true,
                'q' => self.state.can_castle_queen_side_white = true,
                '-' => {
                    self.state.can_castle_king_side_black = false;
                    self.state.can_castle_queen_side_black = false;
                    self.state.can_castle_king_side_white = false;
                    self.state.can_castle_queen_side_white = false;
                }
                _ => unreachable!(),
            }
        }

        // 4. Set last double move
        self.state.did_double_move = match fields.next() {
            Some("-") => None,
            Some(x) => Some(Engine::algebraic_notation_to_position(x)),
            _ => unreachable!(),
        };

        // 5. Set half moves and full moves
        self.state.half_moves = fields.next().unwrap().parse().unwrap();
        self.state.full_moves = fields.next().unwrap().parse().unwrap();
    }

    // TODO Add unit tests for this 2 functions
    fn algebraic_notation_to_position(p: &str) -> Position {
        let mut chars = p.chars();
        Position(chars.next().unwrap() as i32 - 'a' as i32, chars.next().unwrap().to_digit(10).unwrap() as i32)
    }
    fn position_to_algebraic_notation(p: Position) -> String {
        format!("{}{}", (b'a' + p.0 as u8) as char, (b'0' + p.1 as u8) as char)
    }

    pub fn get_piece(&self, Position(file, rank): Position) -> &Option<Piece> {
        &self.board[file as usize + rank as usize * 8]
    }

    fn get_piece_mut(&mut self, Position(file, rank): Position) -> &mut Option<Piece> {
        &mut self.board[file as usize + rank as usize * 8]
    }

    fn get_king_position(&self) -> Position {
        if self.state.turn == Color::White {
            self.state.white_king_position
        } else {
            self.state.black_king_position
        }
    }

    fn in_bounds(Position(file, rank): &Position) -> bool {
        (0..8).contains(file) && (0..8).contains(rank)
    }

    fn is_pawn_first_move(&self, Position(_, rank): Position) -> bool {
        (self.state.turn == Color::White && rank == 6) || (self.state.turn == Color::Black && rank == 1)
    }

    fn slider_moves<'a>(&'a self, origin: Position, directions: &'a [Position]) -> impl Iterator<Item = Position> + 'a {
        // TODO FIXME: We should short circuit when we find something. Probably would need a take_until_inclusive only for that one
        directions.iter().flat_map(move |&dir| (1..).map(move |i| origin + dir * i).take_while(Engine::in_bounds))
    }

    fn jumper_moves<'a>(&'a self, origin: Position, offsets: &'a [Position]) -> impl Iterator<Item = Position> + 'a {
        offsets.iter().map(move |&offset| origin + offset).filter(Engine::in_bounds)
    }

    fn to_moves<'a>(&'a self, origin: Position, positions: impl Iterator<Item = Position> + 'a) -> impl Iterator<Item = Move> + 'a {
        positions.map(move |current| Move {
            from: origin,
            to: current,
            kind: MoveKind::Normal,
        })
    }

    fn pawn_moves(&self, origin: Position) -> Vec<Move> {
        let mut res = Vec::new();

        let direction = if self.state.turn == Color::White { Position(0, -1) } else { Position(0, 1) };

        let move_forward = origin + direction;
        let move_forward_two = move_forward + direction;
        let move_left = origin + Position(-1, 0);
        let move_right = origin + Position(1, 0);
        let attack_left = move_forward + Position(-1, 0);
        let attack_right = move_forward + Position(1, 0);
        
        let check_promotion = |target: Position, res: &mut Vec<_>| {
            if target.1 == 0 || target.1 == 7 {
                res.push(Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Promote(PieceKind::Bishop),
                });
                res.push(Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Promote(PieceKind::Rook),
                });
                res.push(Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Promote(PieceKind::Queen),
                });
                res.push(Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Promote(PieceKind::Knight),
                });
            } else {
                res.push(Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Normal,
                });
            }
        };

        if self.get_piece(move_forward).is_none() {
            check_promotion(move_forward, &mut res);
            if self.is_pawn_first_move(origin) && self.get_piece(move_forward_two).is_none() {
                res.push(Move {
                    from: origin,
                    to: move_forward_two,
                    kind: MoveKind::Double,
                });
            }
        }

        if Engine::in_bounds(&attack_left) {
            if let Some(piece) = self.get_piece(attack_left) {
                if piece.color != self.state.turn {
                    check_promotion(attack_left, &mut res);
                }
            } else if self.pawn_did_double_move(move_left) {
                res.push(Move {
                    from: origin,
                    to: attack_left,
                    kind: MoveKind::EnPassant,
                });
            }
        }

        if Engine::in_bounds(&attack_right) {
            if let Some(piece) = self.get_piece(attack_right) {
                if piece.color != self.state.turn {
                    check_promotion(attack_right, &mut res);
                }
            } else if self.pawn_did_double_move(move_right) {
                res.push(Move {
                    from: origin,
                    to: attack_right,
                    kind: MoveKind::EnPassant,
                });
            }
        }

        res
    }

    /// Gets the list of positions where there is a piece protecting the given position from posible danger
    /// This function is not very smart, but is good enough to reduce the number of checks that need to be done to check if a move is valid
    fn get_defendants(&self, position: Position) -> impl Iterator<Item = Position> + '_ {
        let moves = if self.state.turn == Color::White { &WHITE_PAWN_ATTACKS } else { &BLACK_PAWN_ATTACKS };
        // TODO FIXME: Handle en passant both with ally pawn closer or further!
        self.slider_moves(position, &ROOK_DIRECTIONS)
            .chain(self.slider_moves(position, &BISHOP_DIRECTIONS))
            .chain(self.jumper_moves(position, &KNIGHT_JUMPS))
            .chain(self.jumper_moves(position, &KING_JUMPS))
            .chain(self.jumper_moves(position, moves))
            .filter(|position| match self.get_piece(*position) {
                Some(piece) => piece.color == self.state.turn,
                _ => false,
            })
    }

    /// Checks if the position is safe from enemy attacks.
    /// It's intended to be used to check for the safety of the king and also to check that a castling move is valid (the intermediate squares cannot be under attack)
    fn is_position_safe(&self, position: Position) -> bool {
        // TODO: Could we simplify this?

        // Check rooks and queens
        if self
            .slider_moves(position, &ROOK_DIRECTIONS)
            .filter_map(|p| *self.get_piece(p))
            .any(|piece| piece.color != self.state.turn && (piece.kind == PieceKind::Rook || piece.kind == PieceKind::Queen))
        {
            return false;
        }

        // Check bishops and queens
        if self
            .slider_moves(position, &BISHOP_DIRECTIONS)
            .filter_map(|p| *self.get_piece(p))
            .any(|piece| piece.color != self.state.turn && (piece.kind == PieceKind::Bishop || piece.kind == PieceKind::Queen))
        {
            return false;
        }

        // Check knights
        if self
            .jumper_moves(position, &KNIGHT_JUMPS)
            .filter_map(|p| *self.get_piece(p))
            .any(|piece| piece.color != self.state.turn && piece.kind == PieceKind::Knight)
        {
            return false;
        }

        // Check enemy king
        if self
            .jumper_moves(position, &KING_JUMPS)
            .filter_map(|p| *self.get_piece(p))
            .any(|piece| piece.color != self.state.turn && piece.kind == PieceKind::King)
        {
            return false;
        }

        // Check pawns
        let moves = &if self.state.turn == Color::White { WHITE_PAWN_ATTACKS } else { BLACK_PAWN_ATTACKS };
        if self
            .jumper_moves(position, moves)
            .filter_map(|p| *self.get_piece(p))
            .any(|piece| piece.color != self.state.turn && piece.kind == PieceKind::Pawn)
        {
            return false;
        }

        true
    }

    fn pseudo_legal_moves(&self) -> Vec<Move> {
        let mut res = Vec::new();
        for file in 0..8 {
            for rank in 0..8 {
                let position = Position(file, rank);
                let Some(piece) = self.get_piece(position) else {
                    continue;
                };
                if piece.color != self.state.turn {
                    continue;
                }
                match piece.kind {
                    PieceKind::Queen => {
                        res.extend(self.to_moves(position, self.slider_moves(position, &ROOK_DIRECTIONS).chain(self.slider_moves(position, &BISHOP_DIRECTIONS))));
                    }
                    PieceKind::Rook => res.extend(self.to_moves(position, self.slider_moves(position, &ROOK_DIRECTIONS))),
                    PieceKind::Bishop => res.extend(self.to_moves(position, self.slider_moves(position, &BISHOP_DIRECTIONS))),
                    PieceKind::Knight => res.extend(self.to_moves(position, self.jumper_moves(position, &KNIGHT_JUMPS))),
                    PieceKind::Pawn => res.extend(self.pawn_moves(position)),
                    PieceKind::King => {
                        res.extend(self.to_moves(position, self.jumper_moves(position, &KING_JUMPS)));
                        let left1 = position + Position(-1, 0);
                        let left2 = position + Position(-2, 0);
                        let left3 = position + Position(-3, 0);
                        let right1 = position + Position(1, 0);
                        let right2 = position + Position(2, 0);

                        if if self.state.turn == Color::White {
                            self.state.can_castle_king_side_white
                        } else {
                            self.state.can_castle_king_side_black
                        } && self.get_piece(right1).is_none()
                            && self.get_piece(right2).is_none()
                            && self.is_position_safe(right1)
                            && self.is_position_safe(right2)
                            && self.is_position_safe(self.get_king_position())
                        {
                            res.push(Move {
                                from: position,
                                to: right2,
                                kind: MoveKind::Castle(Side::King),
                            });
                        }
                        if if self.state.turn == Color::White {
                            self.state.can_castle_queen_side_white
                        } else {
                            self.state.can_castle_queen_side_black
                        } && self.get_piece(left1).is_none()
                            && self.get_piece(left2).is_none()
                            && self.get_piece(left3).is_none()
                            && self.is_position_safe(left1)
                            && self.is_position_safe(left2)
                            && self.is_position_safe(self.get_king_position())
                        {
                            res.push(Move {
                                from: position,
                                to: left2,
                                kind: MoveKind::Castle(Side::Queen),
                            });
                        }
                    }
                }
            }
        }
        res
    }

    pub fn legal_moves(&mut self) -> Outcome {
        // 1. Calculate list of pseudo legal moves
        let pseudo_legal_moves = self.pseudo_legal_moves();

        // 2. See if we are in check. This is necessary to know if a move is valid or not
        let in_check = !self.is_position_safe(self.get_king_position());

        // 3. Get the list of pieces defending the king. This will be used for optimization purposes
        let mut defenders = self.get_defendants(self.get_king_position()).collect::<HashSet<Position>>();
        defenders.insert(if self.state.turn == Color::White {
            self.state.white_king_position
        } else {
            self.state.black_king_position
        });

        // 4. Remove the moves that leave the king unprotected, either by not covering the current check or creating a new check
        let legal_moves = clean(pseudo_legal_moves, |m| !(in_check || defenders.contains(&m.from)) || !self.uncovers_king(*m));

        // 5. Calculate the outcome
        if legal_moves.is_empty() {
            if in_check {
                Outcome::Checkmate
            } else {
                Outcome::Drowned
            }
        } else {
            Outcome::Continue(legal_moves)
        }
    }

    fn uncovers_king(&mut self, movement: Move) -> bool {
        #[cfg(debug_assertions)]
        let board = self.board;

        // 1. Set up new board by doing the movement
        let undo = self.make_undoable_move(movement);

        // 2. Checks the king's safety
        let is_kind_safe = self.is_position_safe(self.get_king_position());

        // 3. Restores the board
        self.undo_move(undo);

        #[cfg(debug_assertions)]
        if board != self.board {
            println!("{movement:?}")
        }

        !is_kind_safe
    }

    /// Makes the move on the board and returns the captured piece if any
    pub fn make_move(&mut self, Move { from, to, kind }: Move) -> Option<Piece> {
        // 1. Update state regarding castling rights
        self.state.did_double_move = None;
        if from == self.state.white_king_position {
            self.state.white_king_position = to;
            self.state.can_castle_king_side_white = false;
            self.state.can_castle_queen_side_white = false;
        } else if from == self.state.black_king_position {
            self.state.black_king_position = to;
            self.state.can_castle_king_side_black = false;
            self.state.can_castle_queen_side_black = false;
        } else if from == Position(0, 0) {
            self.state.can_castle_queen_side_black = false;
        } else if from == Position(7, 0) {
            self.state.can_castle_king_side_black = false;
        } else if from == Position(0, 7) {
            self.state.can_castle_queen_side_white = false;
        } else if from == Position(7, 7) {
            self.state.can_castle_king_side_white = false;
        }

        // 2. Toggle turn
        self.state.turn = self.state.turn.other();

        // 3. Move pieces and keep track of captured piece if any
        let mut captured = *self.get_piece(to);

        // Case specify logic
        match kind {
            MoveKind::Double => {
                self.state.did_double_move = Some(to);
            }
            MoveKind::Castle(side) => match side {
                Side::Queen => *self.get_piece_mut(Position(3, to.1)) = self.get_piece_mut(Position(0, to.1)).take(),
                Side::King => *self.get_piece_mut(Position(5, to.1)) = self.get_piece_mut(Position(7, to.1)).take(),
            },
            MoveKind::EnPassant => {
                // In this case we override the captured piece because en passant is a fucking nightmare 🙂
                captured = self.get_piece_mut(Position(to.0, from.1)).take();
            }
            MoveKind::Promote(kind) => {
                self.get_piece_mut(from).as_mut().unwrap().kind = kind;
            }
            _ => (),
        };

        // Shared logic
        *self.get_piece_mut(to) = self.get_piece_mut(from).take();

        captured
    }

    pub fn make_undoable_move(&mut self, movement: Move) -> Undo {
        let old_state = self.state;
        let captured = self.make_move(movement);
        Undo { old_state, captured, movement }
    }

    /// Undoes a move resetting the state and undoing captures
    pub fn undo_move(&mut self, Undo { old_state, captured, movement }: Undo) {
        // 1. Move the piece back into position
        *self.get_piece_mut(movement.from) = self.get_piece_mut(movement.to).take();

        // 2. Move specific logic
        match movement.kind {
            MoveKind::Normal => {
                *self.get_piece_mut(movement.to) = captured;
            }
            MoveKind::EnPassant => {
                *self.get_piece_mut(Position(movement.to.0, movement.from.1)) = Some(Piece {
                    kind: PieceKind::Pawn,
                    color: old_state.turn.other(),
                });
            }
            MoveKind::Castle(side) => match side {
                Side::King => *self.get_piece_mut(Position(7, movement.to.1)) = self.get_piece_mut(Position(5, movement.to.1)).take(),
                Side::Queen => *self.get_piece_mut(Position(0, movement.to.1)) = self.get_piece_mut(Position(3, movement.to.1)).take(),
            },
            MoveKind::Promote(_) => {
                self.get_piece_mut(movement.from).as_mut().unwrap().kind = PieceKind::Pawn;
                *self.get_piece_mut(movement.to) = captured;
            }
            MoveKind::Double => (),
        };

        // 3. Reset the state
        self.state = old_state;
    }

    /// Checks if the piece at the position `p` did a double move last turn
    fn pawn_did_double_move(&self, p: Position) -> bool {
        self.state.did_double_move == Some(p)
    }
}
