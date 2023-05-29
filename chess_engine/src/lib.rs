#![feature(return_position_impl_trait_in_trait)]
use std::collections::HashMap;
mod model;
pub use model::*;
mod test;

// TODO move constants to it's own file and the data model to another one
pub const PIECE_SIZE: i32 = 45;
pub const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
pub const ROOK_DIRECTIONS: [Direction; 4] = [Direction(-1, 0), Direction(1, 0), Direction(0, 1), Direction(0, -1)];
const BISHOP_DIRECTIONS: [Direction; 4] = [Direction(1, 1), Direction(1, -1), Direction(-1, 1), Direction(-1, -1)];
const KNIGHT_JUMPS: [Direction; 8] = [
    Direction(1, 2),
    Direction(-1, 2),
    Direction(1, -2),
    Direction(-1, -2),
    Direction(2, 1),
    Direction(2, -1),
    Direction(-2, 1),
    Direction(-2, -1),
];
const KING_JUMPS: [Direction; 8] = [
    Direction(1, 0),
    Direction(1, -1),
    Direction(0, -1),
    Direction(-1, -1),
    Direction(-1, 0),
    Direction(-1, 1),
    Direction(0, 1),
    Direction(1, 1),
];
const WHITE_PAWN_ATTACKS: [Direction; 2] = [Direction(-1, -1), Direction(1, -1)];
const BLACK_PAWN_ATTACKS: [Direction; 2] = [Direction(-1, 1), Direction(1, 1)];

trait Filter<T> {
    /// Removes the elements in `self` for which `f` returns false and returns itself
    fn filter(self, f: impl FnMut(&T) -> bool) -> Self;
}

impl<T> Filter<T> for Vec<T> {
    fn filter(mut self, mut f: impl FnMut(&T) -> bool) -> Vec<T> {
        let mut idx = 0;
        while idx < self.len() {
            if !f(&self[idx]) {
                self.swap_remove(idx);
            } else {
                idx += 1;
            }
        }
        self
    }
}

pub trait MyIters: Iterator {
    fn to_moves(self, origin: Position) -> impl Iterator<Item = Move>
    where
        Self: std::marker::Sized,
        Self: Iterator<Item = Position>,
    {
        self.map(move |current| Move {
            from: origin,
            to: current,
            kind: MoveKind::Normal,
        })
    }
}

impl<T: ?Sized> MyIters for T where T: Iterator {}

pub struct Engine {
    pub board: [Option<Piece>; 8 * 8],
    pub state: State,
}

impl Engine {
    pub fn new(fen: &str) -> Self {
        let mut fields = fen.split_ascii_whitespace();

        // 1. Set piece positions
        let mut board = [None; 8 * 8];
        let mut white_king_position = Position(0, 0);
        let mut black_king_position = Position(0, 0);
        let mut file = 0;
        let mut rank = 0;
        for c in fields.next().unwrap().chars() {
            if c == '/' {
                file = 0;
                rank += 1;
            } else if let Some(n) = c.to_digit(10) {
                file += n as i32;
            } else {
                let color = if c.is_uppercase() { Color::White } else { Color::Black };
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
                if color == Color::White && kind == PieceKind::King {
                    white_king_position = position
                } else if color == Color::Black && kind == PieceKind::King {
                    black_king_position = position
                }
                board[position.to_index()] = Some(Piece { kind, color });
                file += 1;
            }
        }

        // 2. Set turn
        let turn = match fields.next() {
            Some("w") => Color::White,
            Some("b") => Color::Black,
            _ => unreachable!(),
        };

        // 3. Set castling rights
        let mut can_castle_king_side_black = false;
        let mut can_castle_queen_side_black = false;
        let mut can_castle_king_side_white = false;
        let mut can_castle_queen_side_white = false;
        for c in fields.next().unwrap().chars() {
            match c {
                'K' => can_castle_king_side_white = true,
                'Q' => can_castle_queen_side_white = true,
                'k' => can_castle_king_side_black = true,
                'q' => can_castle_queen_side_black = true,
                '-' => (),
                _ => unreachable!(),
            }
        }

        // 4. Set last double move
        let did_double_move = match fields.next() {
            Some("-") => None,
            Some(x) => Some(Engine::algebraic_notation_to_position(x)),
            _ => unreachable!(),
        };

        // 5. Set half moves and full moves
        let half_moves = fields.next().unwrap().parse().unwrap();
        let full_moves = fields.next().unwrap().parse().unwrap();

        Self {
            board,
            state: State {
                can_castle_queen_side_white,
                can_castle_king_side_white,
                can_castle_queen_side_black,
                can_castle_king_side_black,
                white_king_position,
                black_king_position,
                did_double_move,
                half_moves,
                full_moves,
                turn,
            },
        }
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

    // TODO Add unit tests for this 2 functions
    fn algebraic_notation_to_position(p: &str) -> Position {
        let mut chars = p.chars();
        Position(chars.next().unwrap() as i32 - 'a' as i32, chars.next().unwrap().to_digit(10).unwrap() as i32)
    }
    fn position_to_algebraic_notation(p: Position) -> String {
        format!("{}{}", (b'a' + p.0 as u8) as char, (b'8' - p.1 as u8) as char)
    }
    fn move_to_algebraic_notation(Move { from, to, kind }: Move) -> String {
        format!(
            "{}{}{}",
            Engine::position_to_algebraic_notation(from),
            Engine::position_to_algebraic_notation(to),
            match kind {
                MoveKind::Promote(p) => match p {
                    PieceKind::Rook => "r",
                    PieceKind::Knight => "n",
                    PieceKind::Bishop => "b",
                    PieceKind::Queen => "q",
                    _ => "",
                },
                _ => "",
            }
        )
    }

    pub fn get_piece(&self, pos: Position) -> &Option<Piece> {
        &self.board[pos.to_index()]
    }

    fn get_piece_mut(&mut self, pos: Position) -> &mut Option<Piece> {
        &mut self.board[pos.to_index()]
    }

    fn king_position(&self) -> Position {
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

    pub fn slider_moves<'a>(&'a self, origin: Position, directions: &'a [Direction]) -> impl Iterator<Item = Position> + 'a {
        let mut should_continue = true;
        directions.iter().flat_map(move |&dir| {
            (1..).map(move |i| origin + dir * i).take_while(Engine::in_bounds).take_while(move |position| {
                if should_continue {
                    if let Some(piece) = self.get_piece(*position) {
                        should_continue = false;
                        // If it is an enemy piece we care about it
                        return piece.color != self.state.turn;
                    }
                }

                should_continue
            })
        })
    }

    fn jumper_moves<'a>(&'a self, origin: Position, offsets: &'a [Direction]) -> impl Iterator<Item = Position> + 'a {
        offsets
            .iter()
            .map(move |&offset| origin + offset)
            .filter(|pos| Engine::in_bounds(pos) && self.get_piece(*pos).map_or(true, |piece| piece.color != self.state.turn))
    }

    // TODO return an iterator with the generator crate
    fn pawn_moves(&self, origin: Position) -> Vec<Move> {
        // We are going to collect all the possible pawn moves in this vector
        let mut res = Vec::new();

        let forward = self.state.turn.forward();

        // NOTE: Most pawn moves could lead to a promotion so we encapsulate that logic inside this lambda to reuse it
        let check_promotion = |target: Position, res: &mut Vec<_>| {
            if target.1 == 0 || target.1 == 7 {
                res.extend([PieceKind::Bishop, PieceKind::Rook, PieceKind::Queen, PieceKind::Knight].iter().map(|&k| Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Promote(k),
                }));
            } else {
                res.push(Move {
                    from: origin,
                    to: target,
                    kind: MoveKind::Normal,
                });
            }
        };

        // 1. Move forward and double move on first move
        if self.get_piece(origin + forward).is_none() {
            check_promotion(origin + forward, &mut res);
            if self.is_pawn_first_move(origin) && self.get_piece(origin + (forward * 2)).is_none() {
                res.push(Move {
                    from: origin,
                    to: origin + (forward * 2),
                    kind: MoveKind::Double,
                });
            }
        }

        // 2. Attack left or en passant left
        let attack_left = origin + forward + Direction::LEFT;
        if Engine::in_bounds(&attack_left) {
            if let Some(piece) = self.get_piece(attack_left) {
                if piece.color != self.state.turn {
                    check_promotion(attack_left, &mut res);
                }
            } else if self.pawn_did_double_move(origin + Direction::LEFT) {
                res.push(Move {
                    from: origin,
                    to: attack_left,
                    kind: MoveKind::EnPassant,
                });
            }
        }

        // 3. Attack right or en passant right
        let attack_right = origin + forward + Direction::RIGHT;
        if Engine::in_bounds(&attack_right) {
            if let Some(piece) = self.get_piece(attack_right) {
                if piece.color != self.state.turn {
                    check_promotion(attack_right, &mut res);
                }
            } else if self.pawn_did_double_move(origin + Direction::RIGHT) {
                res.push(Move {
                    from: origin,
                    to: attack_right,
                    kind: MoveKind::EnPassant,
                });
            }
        }

        res
    }

    /// This function looks for allied pieces that are acting as defenders for the piece in the given `position`
    /// Defenders are those that are stopping attacks from sliders
    /// This function's main purpose is to generate a list of the pieces that need extra checking to validate that moving them will not reveal the king to the enemy
    fn get_defenders(&self, position: Position) -> HashMap<Position, Defense> {
        let mut defendants = HashMap::new();

        // 1) Check the diagonals
        for direction in BISHOP_DIRECTIONS {
            // We keep track of the defender candidate and start looking outwards

            let mut iter = (1..)
                .map(move |i| position + direction * i)
                .take_while(Engine::in_bounds)
                .filter_map(|position| self.get_piece(position).map(|piece| (position, piece)));

            match (iter.next(), iter.next()) {
                (Some((defender_position, defender_piece)), Some((_, attacker_piece)))
                    if defender_piece.color == self.state.turn && attacker_piece.color != self.state.turn && (attacker_piece.kind == PieceKind::Bishop || attacker_piece.kind == PieceKind::Queen) =>
                {
                    defendants.insert(defender_position, Defense::LineBlocker(direction));
                }
                _ => (),
            }
        }

        // FIXME the logic for the en passant could be a problem when dealing with vertical movements?
        for direction in ROOK_DIRECTIONS {
            // We keep track of the defender candidate and start looking outwards
            let mut iter = (1..)
                .map(move |i| position + direction * i)
                .take_while(Engine::in_bounds)
                .filter_map(|position| self.get_piece(position).map(|piece| (position, piece)));

            // TODO simplify the conditions...
            match (iter.next(), iter.next()) {
                (Some((defender_position, defender_piece)), Some((_, attacker_piece)))
                    if defender_piece.color == self.state.turn && attacker_piece.color != self.state.turn && (attacker_piece.kind == PieceKind::Rook || attacker_piece.kind == PieceKind::Queen) =>
                {
                    defendants.insert(defender_position, Defense::LineBlocker(direction));
                }
                (Some((defender_position, defender_piece)), Some((attacker_position, attacker_piece)))
                    if defender_piece.color == self.state.turn
                        && attacker_piece.color != self.state.turn
                        && defender_piece.kind == PieceKind::Pawn
                        && attacker_piece.kind == PieceKind::Pawn
                        && self.pawn_did_double_move(attacker_position)
                        && attacker_position == defender_position + direction
                        && iter
                            .next()
                            .map_or(false, |(_, p)| p.color != self.state.turn && (p.kind == PieceKind::Rook || p.kind == PieceKind::Queen)) =>
                {
                    defendants.insert(defender_position, Defense::DoublePawn(attacker_position));
                }
                (Some((attacker_position, attacker_piece)), Some((defender_position, defender_piece)))
                    if defender_piece.color == self.state.turn
                        && attacker_piece.color != self.state.turn
                        && defender_piece.kind == PieceKind::Pawn
                        && attacker_piece.kind == PieceKind::Pawn
                        && self.pawn_did_double_move(attacker_position)
                        && defender_position == attacker_position + direction
                        && iter
                            .next()
                            .map_or(false, |(_, p)| p.color != self.state.turn && (p.kind == PieceKind::Rook || p.kind == PieceKind::Queen)) =>
                {
                    defendants.insert(defender_position, Defense::DoublePawn(attacker_position));
                }
                _ => (),
            }
        }

        defendants
    }

    /// Checks if the position is safe from enemy attacks.
    /// It's intended to be used to check for the safety of the king and also to check that a castling move is valid (the intermediate squares cannot be under attack)
    fn is_position_safe(&self, position: Position) -> bool {
        self.get_attackers(position).next().is_none()
    }

    fn get_attackers(&self, position: Position) -> impl Iterator<Item = Position> + '_ {
        let rooks_and_queens = self.slider_moves(position, &ROOK_DIRECTIONS).filter(|&pos| {
            self.get_piece(pos)
                .map_or(false, |piece| piece.color != self.state.turn && (piece.kind == PieceKind::Rook || piece.kind == PieceKind::Queen))
        });

        let bishops_and_queens = self.slider_moves(position, &BISHOP_DIRECTIONS).filter(|&pos| {
            self.get_piece(pos)
                .map_or(false, |piece| piece.color != self.state.turn && (piece.kind == PieceKind::Bishop || piece.kind == PieceKind::Queen))
        });

        let knights = self
            .jumper_moves(position, &KNIGHT_JUMPS)
            .filter(|&pos| self.get_piece(pos).map_or(false, |piece| piece.color != self.state.turn && piece.kind == PieceKind::Knight));

        let king = self
            .jumper_moves(position, &KING_JUMPS)
            .filter(|&pos| self.get_piece(pos).map_or(false, |piece| piece.color != self.state.turn && piece.kind == PieceKind::King));

        let moves = if self.state.turn == Color::White { &WHITE_PAWN_ATTACKS } else { &BLACK_PAWN_ATTACKS };
        let pawns = self
            .jumper_moves(position, moves)
            .filter(|&pos| self.get_piece(pos).map_or(false, |piece| piece.color != self.state.turn && piece.kind == PieceKind::Pawn));

        // Look for all the pieces that could be attacking the position and check that there are none
        rooks_and_queens.chain(bishops_and_queens).chain(knights).chain(king).chain(pawns)
    }

    /// Generates all the pseudo legal moves for the current board
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
                        res.extend(self.slider_moves(position, &ROOK_DIRECTIONS).chain(self.slider_moves(position, &BISHOP_DIRECTIONS)).to_moves(position));
                    }
                    PieceKind::Rook => res.extend(self.slider_moves(position, &ROOK_DIRECTIONS).to_moves(position)),
                    PieceKind::Bishop => res.extend(self.slider_moves(position, &BISHOP_DIRECTIONS).to_moves(position)),
                    PieceKind::Knight => res.extend(self.jumper_moves(position, &KNIGHT_JUMPS).to_moves(position)),
                    PieceKind::Pawn => res.extend(self.pawn_moves(position)),
                    PieceKind::King => {
                        res.extend(self.jumper_moves(position, &KING_JUMPS).to_moves(position));
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
                            && self.is_position_safe(self.king_position())
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
                            && self.is_position_safe(self.king_position())
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

    fn move_stop_checks(&self, Move { from: _, to, kind: _ }: &Move, attackers: &[Position]) -> bool {
        // TODO if there are more than 2 attackers the only way to scape that is to move the king?
        attackers.iter().filter_map(|&pos| self.get_piece(pos).map(|piece| (pos, piece))).all(|(pos, piece)| {
            *to == pos
                || match piece.kind {
                    PieceKind::Pawn | PieceKind::Knight | PieceKind::King => false,
                    PieceKind::Rook | PieceKind::Bishop | PieceKind::Queen => (*to - self.king_position()).normalized() == (pos - *to).normalized(),
                }
        })
    }

    fn move_breaks_defense(&self, Move { from, to, kind: _ }: &Move, defenders: &HashMap<Position, Defense>) -> bool {
        match defenders.get(from) {
            Some(Defense::DoublePawn(attacked)) => attacked == to,
            Some(Defense::LineBlocker(dir)) => (*to - self.king_position()).normalized() != *dir,
            None => false,
        }
    }

    /// Generates all the legal moves for the current board
    /// TODO return an iterator, that could be then converted to a Map<MoveString, Move> so that when we receive a move request like a2a4 we can search for the move in the list of valid moves
    pub fn legal_moves(&mut self) -> (Vec<Move>, Outcome) {
        // 1. Calculate list of pseudo legal moves
        let pseudo_legal_moves = self.pseudo_legal_moves();

        // 2. See if we are in check. This is necessary to know if a move is valid or not
        let in_check = !self.is_position_safe(self.king_position());

        // 3. Get the list of pieces defending the king. This will be used for optimization purposes
        let defenders = self.get_defenders(self.king_position());

        // 4. Remove the moves that leave the king unprotected, either by not covering the current check or creating a new check
        // SPEED: It would be great if we did not have to create the full list of moves to remove the ilegal ones latter...
        // This could be done with iterators, but only if the `uncovers_king` check does not borrow the board mutably
        // TODO Maybe turn this into it's own function

        let attackers = self.get_attackers(self.king_position()).collect::<Vec<_>>();

        let is_move_legal = |movement: &Move| {
            if movement.from == self.king_position() {
                // 1. Check if, for any of the attackers, we are moving along it's axis of attack without capturing them
                let moving_along_attacked_axis = attackers.iter().filter_map(|&pos| self.get_piece(pos).map(|piece| (pos, piece))).any(|(pos, piece)| match piece.kind {
                    PieceKind::Pawn | PieceKind::Knight | PieceKind::King => false,
                    PieceKind::Rook | PieceKind::Bishop | PieceKind::Queen => (movement.to - pos).normalized() == (movement.from - pos).normalized(),
                });

                // 2. Check that the new position is safe from new attackers
                !moving_along_attacked_axis && self.is_position_safe(movement.to)
            } else {
                !self.move_breaks_defense(movement, &defenders) && self.move_stop_checks(movement, &attackers)
            }
        };

        let legal_moves = pseudo_legal_moves.filter(is_move_legal);

        // 5. Calculate the outcome (TODO Maybe do this somewhere else?)
        if legal_moves.is_empty() {
            if in_check {
                ([].into(), Outcome::Checkmate)
            } else {
                ([].into(), Outcome::Drowned)
            }
        } else {
            (legal_moves, Outcome::Continue)
        }
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
        } else if from == Position(0, 0) || to == Position(0, 0) {
            self.state.can_castle_queen_side_black = false;
        } else if from == Position(7, 0) || to == Position(7, 0) {
            self.state.can_castle_king_side_black = false;
        } else if from == Position(0, 7) || to == Position(0, 7) {
            self.state.can_castle_queen_side_white = false;
        } else if from == Position(7, 7) || to == Position(7, 7) {
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

    /// Makes the move while returning the information needed to undo it
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
