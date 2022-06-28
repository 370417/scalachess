package chess
package format

import cats.data.Validated

import chess.variant.Variant

object UciDump {

  // a2a4, b8c6
  def apply(force960Notation: Boolean)(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant, force960Notation)

  def apply(
      moves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant,
      force960Notation: Boolean = false
  ): Validated[String, List[String]] =
    if (moves.isEmpty) Validated.valid(Nil)
    else Replay(moves, initialFen, variant) andThen (_.valid) map apply(force960Notation)

  def move(variant: Variant, force960Notation: Boolean = false)(action: Action): String =
    action match {
      case m: Move =>
        m.castle.fold(m.toUci.uci) {
          case ((kf, kt), (rf, _))
              if force960Notation || kf == kt || variant.chess960 || variant.fromPosition =>
            kf.key + rf.key
          case ((kf, kt), _) => kf.key + kt.key
        }
      case d: Drop => d.toUci.uci
      case p: Pass => p.toUci.uci
    }
}
