import { useState } from 'react';
import Board from './Board';
import Home from './Home';

const Index = () => {
	const [gameId, setGameId] = useState<null | string>(null)

	return (
		gameId == null ?
			<Home setGameID={setGameId} />
			:
			<Board gameId={gameId} />
	)
}

export default Index;
