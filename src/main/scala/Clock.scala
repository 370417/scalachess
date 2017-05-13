package chess

import java.text.DecimalFormat

import Clock.Config

// All unspecified durations are expressed in seconds
case class Clock(
    config: Config,
    color: Color,
    players: Color.Map[ClockPlayer],
    timer: Option[Timestamp] = None
) {
  import Timestamp.now

  @inline private def pending(c: Color) = timer match {
    case Some(t) if c == color => t.toNow
    case _ => Centis(0)
  }

  @inline private def rawRemaining(c: Color) =
    players(c).remaining - pending(c)

  def remainingTime(c: Color) = rawRemaining(c) nonNeg

  def outOfTimeWithGrace(c: Color) = players(c).remainingGrace < pending(c)

  def moretimeable(c: Color) = rawRemaining(c).centis < 100 * 60 * 60 * 2

  def isInit = players.forall(_.isInit)

  def isRunning = timer.isDefined

  def start = if (isRunning) this else copy(timer = Some(now))

  def stop = timer.fold(this) { t =>
    copy(
      players = players.update(color, _.takeTime(t.toNow)),
      timer = None
    )
  }

  def updatePlayer(c: Color)(f: ClockPlayer => ClockPlayer) =
    copy(players = players.update(c, f))

  def switch = copy(
    color = !color,
    timer = timer.map(_ => now)
  )

  def step(
    metrics: MoveMetrics = MoveMetrics(),
    withInc: Boolean = true
  ) = timer.map { t =>
    val elapsed = t.toNow

    val player = players(color)

    val reportedLag = metrics.reportedLag(elapsed)

    val lagComp = reportedLag.fold(Centis(0)) { _ atMost ClockPlayer.maxLagComp }

    val inc = if (withInc) player.increment else Centis(0)

    updatePlayer(color) {
      _.takeTime(((elapsed - lagComp) nonNeg) - inc)
        .copy(lag = reportedLag)
    }.switch
  }

  // To do: safely add this to takeback to remove inc from player.
  // def deinc = updatePlayer(color, _.giveTime(-incrementOf(color)))

  def takeback = switch

  def giveTime(c: Color, t: Centis) = updatePlayer(c) {
    _.giveTime(t)
  }

  def setRemainingTime(c: Color, centis: Centis) = updatePlayer(c) {
    _.setRemaining(centis)
  }

  def incrementOf(c: Color) = players(c).increment

  def goBerserk(c: Color) = updatePlayer(c) { _.copy(berserk = true) }

  def berserked(c: Color) = players(c).berserk
  def lag(c: Color) = players(c).lag

  def estimateTotalSeconds = config.estimateTotalSeconds
  def estimateTotalTime = config.estimateTotalTime
  def increment = config.increment
  def incrementSeconds = config.incrementSeconds
  def limit = config.limit
  def limitInMinutes = config.limitInMinutes
  def limitSeconds = config.limitSeconds
}

case class ClockPlayer(
    config: Clock.Config,
    elapsed: Centis = Centis(0),
    lag: Option[Centis] = None,
    berserk: Boolean = false
) {
  import ClockPlayer._

  val limit = {
    if (berserk) config.initTime - config.berserkPenalty
    else config.initTime
  }

  def isInit = elapsed.centis == 0

  def remaining = limit - elapsed

  def takeTime(t: Centis) = copy(elapsed = elapsed + t)

  def giveTime(t: Centis) = takeTime(-t)

  def setRemaining(t: Centis) = copy(elapsed = limit - t)

  def remainingGrace = lag.fold(remaining)(l => remaining + ((l * 2) atMost maxGrace))

  def increment = if (berserk) Centis(0) else config.increment
}

object ClockPlayer {
  val maxLagComp = Centis(100)
  val maxGrace = Centis(100)
}

object Clock {
  private val limitFormatter = new DecimalFormat("#.##")

  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int) {

    def berserkable = incrementSeconds == 0 || limitSeconds > 0

    def emergSeconds = math.min(60, math.max(10, limitSeconds / 8))

    def estimateTotalSeconds = limitSeconds + 40 * incrementSeconds

    def estimateTotalTime = Centis.ofSeconds(estimateTotalSeconds)

    def hasIncrement = incrementSeconds > 0

    def increment = Centis.ofSeconds(incrementSeconds)

    def limit = Centis.ofSeconds(limitSeconds)

    def limitInMinutes = limitSeconds / 60d

    def toClock = Clock(this)

    def limitString = limitSeconds match {
      case l if l % 60 == 0 => l / 60
      case 15 => "¼"
      case 30 => "½"
      case 45 => "¾"
      case 90 => "1.5"
      case _ => limitFormatter.format(limitSeconds / 60d)
    }

    def show = toString

    override def toString = s"$limitString+$incrementSeconds"

    def berserkPenalty =
      if (limitSeconds < 40 * incrementSeconds) Centis(0)
      else Centis(limitSeconds * (100 / 2))

    def initTime = {
      if (limitSeconds == 0) increment atLeast Centis(300)
      else limit
    }
  }

  // [TimeControl "600+2"] -> 10+2
  def readPgnConfig(str: String): Option[Config] = str.split('+') match {
    case Array(initStr, incStr) => for {
      init <- parseIntOption(initStr)
      inc <- parseIntOption(incStr)
    } yield Config(init, inc)
    case _ => none
  }

  def apply(limit: Int, increment: Int): Clock = apply(Config(limit, increment))

  def apply(config: Config): Clock = {
    Clock(
      config = config,
      color = White,
      players = Color.Map(_ => ClockPlayer(config = config)),
      timer = None
    )
  }
}
