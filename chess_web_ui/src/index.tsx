/* @refresh reload */
import { createSignal, Match, Suspense, Switch } from 'solid-js';
import { render } from 'solid-js/web';

import Board from './Board';
import Home from './Home';

render(() => {
	const [gameId, setGameId] = createSignal(null)

	return (
		<Switch>
			<Match when={gameId() == null}>
				<Home setGameID={setGameId} />
			</Match>
			<Match when={gameId() != null}>
				<Board gameId={gameId} />
			</Match>
		</Switch >
	)
}, document.getElementById('root') as HTMLElement);
