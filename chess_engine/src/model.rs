use derive_more::{Add, AddAssign, Div};
use serde::{Deserialize, Serialize};

use std::{
    ops::{Add, Mul, Sub},
};


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

    pub fn forward(self) -> Direction {
        if self == Color::White {
            Direction::DOWN
        } else {
            Direction::UP
        }
    }
}

// TODO give names instead of using just tuple structs
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Add, AddAssign, Serialize, Deserialize)]
pub struct Position(pub i32, pub i32);

impl Position {
    pub fn to_index(&self) -> usize {
        self.0 as usize + self.1 as usize * 8
    }
}

impl Sub<Position> for Position {
    type Output = Direction;

    fn sub(self, rhs: Position) -> Self::Output {
        Direction(self.0 - rhs.0, self.1 - rhs.1)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Div, Add, AddAssign, Serialize, Deserialize)]
pub struct Direction(pub i32, pub i32);

impl Mul<i32> for Direction {
    type Output = Direction;
    fn mul(self, rhs: i32) -> Self::Output {
        Direction(self.0 * rhs, self.1 * rhs)
    }
}

impl Direction {
    pub const RIGHT: Direction = Direction(1, 0);
    pub const LEFT: Direction = Direction(-1, 0);
    pub const UP: Direction = Direction(0, 1);
    pub const DOWN: Direction = Direction(0, -1);

    pub fn normalized(self) -> Self {
        let gcd = gcd(self.0, self.1);
        self / gcd
    }
}

impl Add<Direction> for Position {
    type Output = Position;
    fn add(self, Direction(a, b): Direction) -> Self::Output {
        Position(self.0 + a, self.1 + b)
    }
}

pub fn gcd(n: i32, m: i32) -> i32 {
    match (n.abs(), m.abs()) {
        (0, 0) => 1,
        (0, res) => res,
        (res, 0) => res,
        (mut n, mut m) => {

            while m != 0 {
                if m < n {
                    std::mem::swap(&mut m, &mut n);
                }
                m %= n;
            }
            n
        }
    }
}


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
    Continue,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct State {
    pub can_castle_queen_side_white: bool,
    pub can_castle_king_side_white: bool,
    pub can_castle_queen_side_black: bool,
    pub can_castle_king_side_black: bool,
    pub white_king_position: Position,
    pub black_king_position: Position,
    pub did_double_move: Option<Position>,
    pub half_moves: usize, // TODO implement half moves
    pub full_moves: usize, // TODO implement full moves
    pub turn: Color,
}



pub enum Defense {
    DoublePawn(Position),
    LineBlocker(Direction),
}
