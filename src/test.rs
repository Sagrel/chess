#[cfg(test)]
mod tests {

    use crate::engine::{Engine, PIECE_SIZE};

    #[profiling::function]
    fn do_all_moves(depth: usize, engine: &mut Engine) -> usize {
        if depth == 0 {
            return 1;
        }

        let mut count = 0;
        for m in engine.calculate_valid_moves() {
            let undo = engine.make_move(m);

            count += do_all_moves(depth - 1, engine);

            engine.undo_move(undo);
        }

        count
    }

    #[test]
    fn from_initial() {
        let (mut rl, thread) = raylib::init().size(PIECE_SIZE * 8, PIECE_SIZE * 8).title("Chess").build();

        #[cfg(feature = "profile-with-tracy")]
        {
            use std::{thread, time::Duration};

            use profiling::tracy_client;
            tracy_client::Client::start();

            profiling::register_thread!("Main Thread");
            thread::sleep(Duration::from_secs(2));
        }
        let mut engine = Engine::new(&mut rl, &thread);

        assert_eq!(20, do_all_moves(1, &mut engine));
        assert_eq!(400, do_all_moves(2, &mut engine));
        assert_eq!(8902, do_all_moves(3, &mut engine));
        assert_eq!(197281, do_all_moves(4, &mut engine));
        assert_eq!(4865609, do_all_moves(5, &mut engine));
        //assert_eq!(119060324, do_all_moves(6, &mut engine));
    }
}
