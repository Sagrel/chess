use axum::{
    extract::{
        ws::{Message, WebSocket},
        WebSocketUpgrade,
    },
    response::IntoResponse,
    routing::get,
    Json, Router,
};
use chess_engine::*;
use serde::Serialize;
use std::{collections::HashMap, net::SocketAddr, sync::Arc};
use tokio::sync::Mutex;
use tower_http::cors::CorsLayer;
use uuid::Uuid;

struct State {
    clients: Mutex<Vec<(Uuid, Uuid)>>,
    games: Mutex<HashMap<Uuid, Engine>>,
}

#[tokio::main]
async fn main() {
    let state = Arc::new(State {
        clients: Mutex::new(Vec::new()),
        games: Mutex::new(HashMap::new()),
    });

    let app = Router::with_state_arc(state)
        .route("/", get(root))
        .route("/initial_position", get(initial_position))
        .route("/ws", get(ws_handler))
        .layer(CorsLayer::permissive());

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    axum::Server::bind(&addr).serve(app.into_make_service()).await.unwrap();
}

// basic handler that responds with a static string
async fn root() -> &'static str {
    "Hello, World!"
}

async fn ws_handler(ws: WebSocketUpgrade) -> impl IntoResponse {
    ws.on_upgrade(handle_socket)
}

async fn handle_socket(mut socket: WebSocket) {
    loop {
        if let Some(msg) = socket.recv().await {
            if let Ok(msg) = msg {
                match msg {
                    Message::Text(t) => {
                        println!("client sent str: {:?}", t);
                        socket.send(Message::Text("Fuck you".to_string())).await.unwrap();
                    }
                    Message::Binary(_) => {
                        println!("client sent binary data");
                    }
                    Message::Ping(_) => {
                        println!("socket ping");
                    }
                    Message::Pong(_) => {
                        println!("socket pong");
                    }
                    Message::Close(_) => {
                        println!("client disconnected");
                        return;
                    }
                }
            } else {
                println!("client disconnected");
                return;
            }
        }
    }
}

// TODO This is ugly as ***
async fn initial_position() -> impl IntoResponse {
    let engine = Engine::new(STARTING_POSITION);
    let board = Board(
        engine
            .board
            .into_iter()
            .map(|piece| {
                piece.map(|p| {
                    (
                        if p.team == Team::White { "white" } else { "black" },
                        match p.kind {
                            PieceKind::Pawn => "pawn",
                            PieceKind::Rook => "rook",
                            PieceKind::Knight => "knight",
                            PieceKind::Bishop => "bishop",
                            PieceKind::Queen => "queen",
                            PieceKind::King => "king",
                        },
                    )
                })
            })
            .collect(),
    );

    Json(board)
}

#[derive(Serialize)]
struct Board<'a>(Vec<Option<(&'a str, &'a str)>>);
