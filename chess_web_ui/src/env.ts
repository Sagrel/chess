export const url = "127.0.0.1:8080";
export const create_game = "http://localhost:8080/create_game";
export const game_exists = (id: string) => `http://localhost:8080/game_exists/${id}`;
export const get_game = (id: string) => `http://localhost:8080/get_game/${id}`;
export const get_moves = (id: string) => `http://localhost:8080/get_moves/${id}`;
export const make_move = (id: string) => `http://localhost:8080/make_move/${id}`;
export const undo_move = (id: string) => `http://localhost:8080/undo_move/${id}`;
export const get_initial_position = "http://localhost:8080/initial_position";
export const ws = "ws://localhost:8080/ws";