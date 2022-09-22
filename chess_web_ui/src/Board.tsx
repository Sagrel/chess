import { Component, createSignal, For, Show } from 'solid-js';

import "./Board.css";
import { get_initial_position, url, ws } from './env';
import Piece from './Piece';
 


const Board: Component = () => {
  const [board, setBoard] = createSignal(Array(8 * 8).fill(null));
  const [selected, setSelected] = createSignal(null);

  fetch(get_initial_position).then((res) => {
    res.json().then((data) => {
      setBoard(data);
    })
  })

  const socket = new WebSocket(ws);

  socket.addEventListener('open', function (event) {
    socket.send('Hello Server!');
  });

  socket.addEventListener('message', (event) => {
    console.log('Message from server ', event.data);

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
                <Piece piece={elem[1]} color={elem[0]}></Piece>
              </Show>
            </div>);
          }}
      </For>
    </div>
  );
};

export default Board;
