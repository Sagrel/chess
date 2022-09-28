import { useEffect, useState } from 'react';

import "./Board.css";
import { get_game, get_moves, make_move, ws } from './env';
import Piece from './Piece';

type Move = {
  from: [number, number],
  to: [number, number],
  kind: MoveKind
}

type MoveKind = "Normal" | "Doble" | "Casstle" | "Enpasant" | "Undo"
type Turn = "White" | "Black"
type Role = Turn | "Espectator"

const positionToIndex = ([file, row]: [number, number]) => {
  return file + row * 8
}


const fetch_get_json = async (url: string) => {
  try {
    const res = await fetch(url)
    return await res.json()
  } catch (e) {
    console.error(e)
  }
}

const fetch_post_json = async (url: string, data: any) => {
  try {
    const res = await fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data),
    })
  } catch (e) {
    console.error(e)
  }
}

const Board = ({ gameId }: { gameId: string }) => {
  const [board, setBoard] = useState(Array(8 * 8).fill(null));
  const [selected, setSelected] = useState<null | number>(null);
  const [hovered, setHovered] = useState<null | number>(null);
  const [moves, setMoves] = useState<[Move] | []>([]);
  const [role, setRole] = useState<Role>("Espectator");
  const [turn, setTurn] = useState<Turn>("White");


  useEffect(
    () => {
      const socket = new WebSocket(ws);

      socket.addEventListener("error", (event) => {
        console.error(event)
      })

      socket.addEventListener('open', (event) => {
        socket.send(gameId);
      });

      socket.addEventListener('message', (event) => {
        const { role, turn, board, moveKind, moves } = JSON.parse(event.data);
        console.log(turn)
        if (role) setRole(role)
        if (turn) setTurn(turn)
        if (board) setBoard(board)
        if (moves) setMoves(moves)
        if (moveKind) {
          new Audio('move.mp3').play();
        }
      });


      return () => {
        socket.close();
      }
    },
    []
  )




  return (
    <div>

      <div className='board' style={{ display: "grid", gridTemplateColumns: "repeat(8, 1fr)", userSelect: "none" }} >
        {board.map((elem, i) => {
          const rank = Math.trunc(i / 8);
          let clas = "square ";

          if ((i + rank) % 2 == 0) {
            clas += "light";
          } else {
            clas += "dark";
          }


          let onClick = () => {
            if (elem?.team == turn && turn == role) { setSelected(i) }
            else { setSelected(null) }
          }
          let onDrop = () => { }
          let onDragEnter = () => { }
          let onDragLeave = () => { }
          let onDragStart = () => { }

          let move = moves.find((elem: Move) => {
            const from = elem.from[0] + elem.from[1] * 8
            const to = elem.to[0] + elem.to[1] * 8
            return from == selected && to == i
          });

          if (selected == i) {
            clas += " selected"
          } else if (move != undefined) {
            clas += board[i] == null ? " movable" : " attacked"
            onClick = () => {
              fetch_post_json(make_move(gameId), move)
              setSelected(null)
            }
          }

          return (<div key={i} onClick={onClick} className={clas} >
            {elem != null && <Piece piece={elem} />}

          </div>);
        })}

      </div >
      <h3>Game id: {gameId}</h3>
    </div>

  );
};

export default Board;
