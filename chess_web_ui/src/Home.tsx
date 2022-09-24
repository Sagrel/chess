
import { useState } from "react";
import { create_game, game_exists } from "./env";

const Home = ({ setGameID }: { setGameID: (value: string) => void }) => {

	const [id, setId] = useState("")

	return (
		<div style={{ display: "flex", flexDirection: "column", width: "100 vw", alignContent: "center" }}>
			<button onClick={async () => {
				let res = await fetch(create_game)
				let data = await res.json()
				setGameID(data)
			}
			}>Create new game</button>
			<div><input value={id} onInput={(e) => setId(e.currentTarget.value)} /><button onClick={async () => {
				let res = await fetch(game_exists(id))
				let data = await res.json()
				if (data == true) {
					setGameID(id)
				}
			}
			}>Join</button></div>
		</div >
	)
}

export default Home;