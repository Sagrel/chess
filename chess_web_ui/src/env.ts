export const create_game = "/api/create_game";
export const game_exists = (id: string) => `/api/game_exists/${id}`;
export const get_game = (id: string) => `/api/get_game/${id}`;
export const get_moves = (id: string) => `/api/get_moves/${id}`;
export const make_move = (id: string) => `/api/make_move/${id}`;
export const undo_move = (id: string) => `/api/undo_move/${id}`;
export const get_initial_position = "/api/initial_position";
export const ws = `ws://${window.location.host}/ws`;