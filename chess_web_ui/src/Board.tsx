import { Component, createEffect, createSignal, For, Show } from 'solid-js';

import "./Board.css";
import { get_game, get_initial_position, ws } from './env';
import Piece from './Piece';



const Board: Component = ({ gameId }: any) => {
  const [board, setBoard] = createSignal(Array(8 * 8).fill(null));
  const [selected, setSelected] = createSignal(null); // TODO set this value in the Piece component
  const [moves, setMoves] = createSignal([]); // TODO get this from the rest api 


  // TODO Get the valid moves to start
  fetch(get_game(gameId)).then((res) => {
    res.json().then((data) => {
      setBoard(data);
    })
  })


  const socket = new WebSocket(ws);

  socket.addEventListener('open', function (event) {
    socket.send(gameId);
  });

  socket.addEventListener('message', (event) => {
    // TODO process event.data to react to oponent accions and make sure to recalculate the valid moves
  });

  return (
    <div class='board' style={{ display: "grid", "grid-template-columns": "repeat(8, 1fr)" }}>
      <For each={board()}>
        {
          (elem, i) => {
            const rank = Math.trunc(i() / 8);
            let clas = "square ";

            if ((i() + rank) % 2 == 0) {
              clas += "light";
            } else {
              clas += "dark";
            }

            return (<div class={clas}>
              <Show when={elem != null}>
                <Piece piece={elem} ></Piece>
              </Show>
            </div>);
          }}
      </For>
    </div>
  );
};

export default Board;
