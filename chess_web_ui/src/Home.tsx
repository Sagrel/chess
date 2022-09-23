import { createSignal } from "solid-js";
import { create_game, game_exists } from "./env";

const Home = ({ setGameID }: any) => {

	const [id, setId] = createSignal("")

	return (
		<div style={{ display: "flex", "flex-direction": "column", width: "100 vw", "align-content": "center" }}>
			<button onClick={async () => {
				let res = await fetch(create_game)
				let data = await res.json()
				setGameID(data)
			}
			}>Create new game</button>
			<div><input value={id()} onInput={(e) => setId(e.currentTarget.value)} /><button onClick={async () => {
				let res = await fetch(game_exists(id()))
				let data = await res.json()
				if (data == true) {
					setGameID(id())
				}
			}
			}>Join</button></div>
		</div>
	)
}

export default Home;