#[cfg(test)]
mod tests {

    use crate::{Engine, STARTING_POSITION};

    fn do_all_moves(depth: usize, engine: &mut Engine) -> usize {
        if depth == 0 {
            return 1;
        }

        let mut count = 0;
        match engine.legal_moves() {
            crate::Outcome::Checkmate | crate::Outcome::Drowned => (),
            crate::Outcome::Continue(movements) => {
                for m in movements {
                    let undo = engine.make_undoable_move(m);

                    count += do_all_moves(depth - 1, engine);

                    engine.undo_move(undo);
                }
            }
        }

        count
    }

    #[test]
    fn from_initial() {
        let mut engine = Engine::new(STARTING_POSITION);

        assert_eq!(20, do_all_moves(1, &mut engine));
        assert_eq!(400, do_all_moves(2, &mut engine));
        assert_eq!(8902, do_all_moves(3, &mut engine));
        assert_eq!(197281, do_all_moves(4, &mut engine));
        assert_eq!(4865609, do_all_moves(5, &mut engine));
        assert_eq!(119060324, do_all_moves(6, &mut engine));
        assert_eq!(3195901860, do_all_moves(7, &mut engine)); // FIXME Fails with 3196082118
    }

    // https://www.chessprogramming.org/Perft_Results#Position_5
    #[test]
    fn from_position_5() {
        let mut engine = Engine::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

        assert_eq!(44, do_all_moves(1, &mut engine));
        assert_eq!(1486, do_all_moves(2, &mut engine));
        assert_eq!(62376, do_all_moves(3, &mut engine));
        assert_eq!(2103487, do_all_moves(4, &mut engine));
        assert_eq!(89941194, do_all_moves(5, &mut engine));
    }
}
