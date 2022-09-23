type PieceKind = "pawn" | "rook" | "bishop" | "queen" | "knight" | "king";
type Color = "white" | "black";


const Piece = (props: { piece: { kind: PieceKind, team: Color } }) => {

	return <img src={"/" + props.piece.team + "-" + props.piece.kind + ".png"} />
}

export default Piece;