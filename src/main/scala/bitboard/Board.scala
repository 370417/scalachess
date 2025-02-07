package chess
package bitboard

import Bitboard.*

// Pieces position on the board
case class Board(
    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    white: Bitboard,
    black: Bitboard,
    occupied: Bitboard
):
  def isOccupied(s: Pos): Boolean = occupied.contains(s)

  def sliders = bishops ^ rooks ^ queens

  lazy val byColor = Color.Map(white, black)

  def byRole(role: Role): Bitboard =
    role match
      case Pawn   => pawns
      case Knight => knights
      case Bishop => bishops
      case Rook   => rooks
      case Queen  => queens
      case King   => kings

  def contains(p: Pos) = occupied.contains(p)

  def roleAt(s: Pos): Option[Role] =
    if pawns.contains(s) then Some(Pawn)
    else if knights.contains(s) then Some(Knight)
    else if bishops.contains(s) then Some(Bishop)
    else if rooks.contains(s) then Some(Rook)
    else if queens.contains(s) then Some(Queen)
    else if kings.contains(s) then Some(King)
    else None

  def colorAt(s: Pos): Option[Color] =
    if white.contains(s) then Some(Color.White)
    else if black.contains(s) then Some(Color.Black)
    else None

  def pieceAt(s: Pos): Option[Piece] =
    for
      color <- colorAt(s)
      role  <- roleAt(s)
    yield Piece(color, role)

  def whiteAt(s: Pos): Boolean =
    colorAt(s).contains(Color.White)

  def blackAt(s: Pos): Boolean =
    colorAt(s).contains(Color.Black)

  def kings(color: Color): List[Pos] =
    (kings & byColor(color)).occupiedSquares

  def attackers(s: Pos, attacker: Color): Bitboard =
    attackers(s, attacker, occupied)

  def attacks(s: Pos, attacker: Color): Boolean =
    attackers(s, attacker).nonEmpty

  def attackers(s: Pos, attacker: Color, occupied: Bitboard): Bitboard =
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.kingAttacks & kings |
        s.pawnAttacks(!attacker) & pawns
    )

  // is a king of this color in check
  def isCheck(color: Color): Check =
    Check(kings(color).exists(attacks(_, !color)))

  /** Find all blockers between the king and attacking sliders First we find all snipers (all potential sliders which
    * can attack the king) Then we loop over those snipers if there is only one blockers between the king and the sniper
    * we add them into the blockers list
    *
    * This is being used when checking a move is safe for the king or not
    */
  def sliderBlockers(us: Color): Bitboard =
    kings(us).headOption.fold(Bitboard.empty) { ourKing =>
      val snipers = byColor(!us) & (
        ourKing.rookAttacks(Bitboard.empty) & (rooks ^ queens) |
          ourKing.bishopAttacks(Bitboard.empty) & (bishops ^ queens)
      )
      val bs = for
        sniper <- snipers.occupiedSquares
        between = Bitboard.between(ourKing, sniper) & occupied
        if !between.moreThanOne
      yield between

      bs.fold(Bitboard.empty)((a, b) => a | b)
    }

  def discard(s: Pos): Board =
    discard(s.bb)

  def discard(mask: Bitboard): Board =
    val notMask = ~mask
    copy(
      pawns = pawns & notMask,
      knights = knights & notMask,
      bishops = bishops & notMask,
      rooks = rooks & notMask,
      queens = queens & notMask,
      kings = kings & notMask,
      white = white & notMask,
      black = black & notMask,
      occupied & notMask
    )

  def updateRole(mask: Bitboard, role: Role): Role => Bitboard =
    case Pawn if role == Pawn     => pawns ^ mask
    case Knight if role == Knight => knights ^ mask
    case Bishop if role == Bishop => bishops ^ mask
    case Rook if role == Rook     => rooks ^ mask
    case Queen if role == Queen   => queens ^ mask
    case King if role == King     => kings ^ mask
    case _                        => roles(role)

  def roles: Role => Bitboard =
    case Pawn   => pawns
    case Knight => knights
    case Bishop => bishops
    case Rook   => rooks
    case Queen  => queens
    case King   => kings

  def updateColor(mask: Bitboard, color: Color): Color => Bitboard =
    case Color.White if color == Color.White => white ^ mask
    case Color.Black if color == Color.Black => black ^ mask
    case _                                   => byColor(color)

  def hasPiece(at: Pos): Boolean =
    colorAt(at).isDefined

  def take(at: Pos): Option[Board] =
    hasPiece(at) option discard(at)

  def put(piece: Piece, at: Pos): Option[Board] =
    !hasPiece(at) option putOrReplace(at, piece) // todo no need to discard

  def putOrReplace(s: Pos, role: Role, color: Color): Board =
    val b = discard(s)
    val m = s.bb
    b.copy(
      pawns = b.updateRole(m, Pawn)(role),
      knights = b.updateRole(m, Knight)(role),
      bishops = b.updateRole(m, Bishop)(role),
      rooks = b.updateRole(m, Rook)(role),
      queens = b.updateRole(m, Queen)(role),
      kings = b.updateRole(m, King)(role),
      white = b.updateColor(m, Color.White)(color),
      black = b.updateColor(m, Color.Black)(color),
      occupied = b.occupied ^ m
    )

  // put a piece into the board
  // remove the existing piece at that pos if needed
  def putOrReplace(s: Pos, p: Piece): Board =
    putOrReplace(s, p.role, p.color)

  // move without capture
  // TODO test
  def move(orig: Pos, dest: Pos): Option[Board] =
    if hasPiece(dest) then None
    else pieceAt(orig).flatMap(p => discard(orig).put(p, dest))

  // TODO test
  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for
      piece <- pieceAt(orig)
      takenPos = taking getOrElse dest
      if hasPiece(takenPos)
      newBoard <- discard(orig).discard(takenPos).put(piece, dest)
    yield newBoard

  // TODO test
  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { c =>
    color(c).occupiedSquares.toSet
  }

  // TODO test
  inline def hasPiece(inline p: Piece) =
    piece(p).nonEmpty

  // TODO remove unsafe get
  // we believe in the integrity of bitboard
  // tests pieceMap . fromMap = identity
  lazy val pieceMap: Map[Pos, Piece] =
    occupied.occupiedSquares.view.map(s => (s, pieceAt(s).get)).toMap

  def piecesOf(c: Color): Map[Pos, Piece] =
    color(c).occupiedSquares.view.map(s => (s, pieceAt(s).get)).toMap

  def pieces: List[Piece] = pieces(occupied)

  def pieces(occupied: Bitboard): List[Piece] =
    occupied.occupiedSquares.flatMap(pieceAt)

  def color(c: Color): Bitboard = c.fold(white, black)

  def piece(p: Piece): Bitboard = color(p.color) & byRole(p.role)

  // guess unmovedRooks from board
  // we assume rooks are on their initial position
  def defaultUnmovedRooks: UnmovedRooks =
    val wr = rooks & white & Bitboard.rank(White.backRank)
    val br = rooks & black & Bitboard.rank(Black.backRank)
    UnmovedRooks(wr | br)

object Board:
  val empty = Board(
    pawns = Bitboard.empty,
    knights = Bitboard.empty,
    bishops = Bitboard.empty,
    rooks = Bitboard.empty,
    queens = Bitboard.empty,
    kings = Bitboard.empty,
    white = Bitboard.empty,
    black = Bitboard.empty,
    occupied = Bitboard.empty
  )
  val standard = Board(
    pawns = Bitboard(0xff00000000ff00L),
    knights = Bitboard(0x4200000000000042L),
    bishops = Bitboard(0x2400000000000024L),
    rooks = Bitboard(0x8100000000000081L),
    queens = Bitboard(0x800000000000008L),
    kings = Bitboard(0x1000000000000010L),
    white = Bitboard(0xffffL),
    black = Bitboard(0xffff000000000000L),
    occupied = Bitboard(0xffff00000000ffffL)
  )

  def fromMap(pieces: PieceMap): Board =
    var pawns    = Bitboard.empty
    var knights  = Bitboard.empty
    var bishops  = Bitboard.empty
    var rooks    = Bitboard.empty
    var queens   = Bitboard.empty
    var kings    = Bitboard.empty
    var white    = Bitboard.empty
    var black    = Bitboard.empty
    var occupied = Bitboard.empty

    pieces.foreach { (s, p) =>
      val position = s.bb
      occupied |= position
      p.role match
        case Pawn   => pawns |= position
        case Knight => knights |= position
        case Bishop => bishops |= position
        case Rook   => rooks |= position
        case Queen  => queens |= position
        case King   => kings |= position

      p.color match
        case Color.White => white |= position
        case Color.Black => black |= position
    }
    Board(pawns, knights, bishops, rooks, queens, kings, white, black, occupied)
