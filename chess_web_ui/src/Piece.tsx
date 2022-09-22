type PieceKind = "pawn" | "rook" | "bishop" | "queen" | "knight" | "king";
type Color = "white" | "black";

import white_king from "./../assets/white-king.png";
import white_queen from "./../assets/white-queen.png";
import white_pawn from "./../assets/white-pawn.png";
import white_rook from "./../assets/white-rook.png";
import white_bishop from "./../assets/white-bishop.png";
import white_knigh from "./../assets/white-knight.png";
const Piece = (props: { piece: PieceKind, color: Color }) => {

	return <img src={"/" + props.color + "-" + props.piece + ".png"} />
}

export default Piece;