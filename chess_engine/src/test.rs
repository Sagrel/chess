#[cfg(test)]
mod tests {

    use crate::{Engine, STARTING_POSITION};

    fn perft(depth: usize, engine: &mut Engine) -> usize {
        let mut count = 0;
        let movements = engine.legal_moves().0;
        for m in movements {
            let undo = engine.make_undoable_move(m);

            let child_moves = if depth <= 1 { 1 } else { bulk_counting(depth - 1, engine) };
            println!("{}: {}", Engine::move_to_algebraic_notation(m), child_moves);
            count += child_moves;

            engine.undo_move(undo);
        }

        count
    }

    fn bulk_counting(depth: usize, engine: &mut Engine) -> usize {
        let mut count = 0;
        let movements = engine.legal_moves().0;
        if depth <= 1 {
            return movements.len();
        }
        for m in movements {
            let temp = engine.board;
            let undo = engine.make_undoable_move(m);

            count += bulk_counting(depth - 1, engine);

            engine.undo_move(undo);
            if engine.board != temp {
                println!("Fuck")
            }
        }

        count
    }

    #[test]
    fn debug() {
        /*
        TODO
        position fen rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8
        go perft 

        */
        let mut engine = Engine::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
        println!("Total {}", perft(4, &mut engine));
    }

    #[test]
    fn from_initial() {
        let mut engine = Engine::new(STARTING_POSITION);

        //assert_eq!(20, bulk_counting(1, &mut engine));
        //assert_eq!(400, bulk_counting(2, &mut engine));
        //assert_eq!(8902, bulk_counting(3, &mut engine));
        //assert_eq!(197281, bulk_counting(4, &mut engine));
        //assert_eq!(4865609, bulk_counting(5, &mut engine));
        //assert_eq!(119060324, bulk_counting(6, &mut engine));
        assert_eq!(3195901860, bulk_counting(7, &mut engine)); // FIXME Fails with 3196082118
    }

    // https://www.chessprogramming.org/Perft_Results#Position_4
    #[test]
    fn from_position_4() {
        let mut engine = Engine::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");

        assert_eq!(6, bulk_counting(1, &mut engine));
        assert_eq!(264, bulk_counting(2, &mut engine));
        assert_eq!(9467, bulk_counting(3, &mut engine));
        assert_eq!(422333, bulk_counting(4, &mut engine));
        assert_eq!(15833292, bulk_counting(5, &mut engine));
    }
    
    // https://www.chessprogramming.org/Perft_Results#Position_5
    #[test]
    fn from_position_5() {
        let mut engine = Engine::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

        assert_eq!(44, bulk_counting(1, &mut engine));
        assert_eq!(1486, bulk_counting(2, &mut engine));
        assert_eq!(62379, bulk_counting(3, &mut engine));
        assert_eq!(2103487, bulk_counting(4, &mut engine));
        assert_eq!(89941194, bulk_counting(5, &mut engine));
    }

    // https://www.chessprogramming.org/Perft_Results#Position_6
    #[test]
    fn from_position_6() {
        let mut engine = Engine::new("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ");

        assert_eq!(46, bulk_counting(1, &mut engine));
        assert_eq!(2079, bulk_counting(2, &mut engine));
        assert_eq!(89890, bulk_counting(3, &mut engine));
        assert_eq!(3894594, bulk_counting(4, &mut engine));
        assert_eq!(164075551, bulk_counting(5, &mut engine));
    }
}
