type PieceKind = "pawn" | "rook" | "bishop" | "queen" | "knight" | "king";
type Color = "white" | "black";

import "./Piece.css"

const Piece = ({ piece }: { piece: { kind: PieceKind, team: Color } }) => {

	return (
		< img
			src={"/" + piece.team + "-" + piece.kind + ".png"} />

	)
}

export default Piece;