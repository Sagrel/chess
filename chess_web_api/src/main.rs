use axum::{
    extract::{
        ws::{Message, WebSocket},
        Path, State, WebSocketUpgrade,
    },
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use axum_macros::debug_handler;
use chess_engine::*;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::{collections::HashMap, net::SocketAddr, str::FromStr, sync::Arc};
use tokio::sync::Mutex;
use tower_http::cors::CorsLayer;
use uuid::Uuid;

struct Game {
    engine: Engine,
    undo: Vec<UndoAction>,
    listeners: Vec<WebSocket>,
}

struct AppState {
    games: Mutex<HashMap<Uuid, Game>>,
}

#[tokio::main]
async fn main() {
    let state = Arc::new(AppState { games: Mutex::new(HashMap::new()) });
    // TODO Revisit Router::with_state_arc.
    let app = Router::with_state(state)
        .route("/initial_position", get(initial_position))
        .route("/create_game", get(create_game))
        .route("/game_exists/:id", get(game_exists))
        .route("/get_game/:id", get(get_game))
        .route("/get_moves/:id", get(get_moves))
        .route("/make_move/:id", post(make_move))
        .route("/undo_move/:id", post(undo_move))
        .route("/ws", get(ws_handler))
        .layer(CorsLayer::permissive());

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    
    axum::Server::bind(&addr).serve(app.into_make_service()).await.unwrap();
}

async fn ws_handler(ws: WebSocketUpgrade, State(state): State<Arc<AppState>>) -> impl IntoResponse {
    ws.on_upgrade(|socket| handle_socket(socket, state))
}

async fn handle_socket(mut socket: WebSocket, state: Arc<AppState>) {
    println!("client connected");

    if let Some(msg) = socket.recv().await {
        if let Ok(Message::Text(id)) = msg {
            let mut games = state.games.lock().await;
            let game = games.get_mut(&Uuid::from_str(&id).unwrap()).unwrap();
            let role = match game.listeners.len() {
                0 => "White",
                1 => "Black",
                _ => "Espectator",
            };
            println!("client role: {}", role);

            socket
                .send(Message::Text(
                    json!({"board": Vec::from(game.engine.board), "turn": game.engine.state.turn, "moves": game.engine.calculate_valid_moves(), "role":role}).to_string(),
                ))
                .await
                .unwrap();
            game.listeners.push(socket);
        } else {
            println!("client disconnected");
        }
    }
}

async fn initial_position() -> impl IntoResponse {
    let engine = Engine::new(STARTING_POSITION);
    Json(Board(Vec::from(engine.board)))
}

#[debug_handler]
async fn make_move(State(state): State<Arc<AppState>>, Path(id): Path<Uuid>, Json(m): Json<Move>) -> impl IntoResponse {
    if let Some(game) = state.games.lock().await.get_mut(&id) {
        game.undo.push(game.engine.make_move(m));
        for listener in &mut game.listeners {
            listener
                .send(Message::Text(
                    json!({"board": Vec::from(game.engine.board), "turn": game.engine.state.turn, "moveKind": m.kind, "moves": game.engine.calculate_valid_moves()}).to_string(),
                ))
                .await
                .unwrap();
        }
    }

    StatusCode::OK
}

#[debug_handler]
async fn undo_move(State(state): State<Arc<AppState>>, Path(id): Path<Uuid>) -> impl IntoResponse {
    if let Some(game) = state.games.lock().await.get_mut(&id) {
        let undo = game.undo.pop().unwrap();
        game.engine.undo_move(undo);
        for listener in &mut game.listeners {
            listener
                .send(Message::Text(
                    json!({"board": Vec::from(game.engine.board), "turn": game.engine.state.turn, "moveKind": "Undo", "moves": game.engine.calculate_valid_moves()}).to_string(),
                ))
                .await
                .unwrap();
        }
    }

    StatusCode::OK.into_response()
}

#[debug_handler]
async fn get_moves(State(state): State<Arc<AppState>>, Path(id): Path<Uuid>) -> impl IntoResponse {
    if let Some(game) = state.games.lock().await.get_mut(&id) {
        (StatusCode::OK, Json(game.engine.calculate_valid_moves())).into_response()
    } else {
        StatusCode::BAD_REQUEST.into_response()
    }
}

#[debug_handler]
async fn get_game(State(state): State<Arc<AppState>>, Path(id): Path<Uuid>) -> impl IntoResponse {
    let board = if let Some(game) = state.games.lock().await.get_mut(&id) {
        game.engine.board
    } else {
        return StatusCode::EXPECTATION_FAILED.into_response();
    };

    (StatusCode::OK, Json(Board(Vec::from(board)))).into_response()
}

#[debug_handler]
async fn create_game(State(state): State<Arc<AppState>>) -> impl IntoResponse {
    let id = Uuid::new_v4();

    state.games.lock().await.insert(
        id,
        Game {
            engine: Engine::new(STARTING_POSITION),
            undo: Vec::new(),
            listeners: Vec::new(),
        },
    );

    (StatusCode::OK, Json(id))
}

#[debug_handler]
async fn game_exists(State(state): State<Arc<AppState>>, Path(id): Path<Uuid>) -> impl IntoResponse {
    let response = state.games.lock().await.contains_key(&id);

    (StatusCode::OK, Json(response))
}

#[derive(Serialize, Deserialize)]
struct Board(Vec<Option<Piece>>);
