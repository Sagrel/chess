use std::{collections::HashMap, sync::Mutex};

use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder};
use chess_engine::*;

struct AppState {
    engines: Mutex<HashMap<String, Engine>>,
}

// TODO Sitch to axum and use web sockets: https://docs.rs/axum/latest/axum/extract/ws/index.html

#[get("/{id}")]
async fn create_or_get(data: web::Data<AppState>, id: web::Path<String>) -> impl Responder {
    let mut engines = data.engines.lock().unwrap();
    let id = id.to_string();
    let fen = if engines.contains_key(&id) {
        engines[&id].get_fen()
    } else {
        engines.insert(id, Engine::new(STARTING_POSITION));
        STARTING_POSITION.to_string()
    };

    HttpResponse::Ok().body(fen)
}
#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let state = AppState { engines: Mutex::new(HashMap::new()) };

    HttpServer::new(move || App::new().app_data(web::Data::new(state.clone())).service(create_or_get))
        .bind(("127.0.0.1", 8080))?
        .run()
        .await
}
