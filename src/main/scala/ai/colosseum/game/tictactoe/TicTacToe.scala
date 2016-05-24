package ai.colosseum.game.tictactoe

import ai.colosseum.game.tictactoe.TicTacToe.XO.{O, X}
import ai.colosseum.model.{Game, Id}

object TicTacToe extends Game {
  type P = Player
  type M = Match
  val name = "tic-tac-toe"
  def play(players: Map[Id[Player], Player]): Match = {
    require(players.size == 2 && players.values.map(_.symbol).toSet == XO.all)
    val xs = players.collectFirst { case (id ,pl) if pl.symbol == X => id }.get
    val os = players.collectFirst { case (id ,pl) if pl.symbol == O => id }.get
    Iterator.iterate(Match.empty(0, xs = xs, os = os)) { m =>
      m.copy(state = m.state.evolve(
        pl = m.currentPlayer,
        p = players(m.currentPlayer).play(m.state)
      ))
    }.dropWhile(_.state.isPlayable).next()
  }
  def score(m: Match): Map[Id[Player], Int] = {
    m.state.board.winner match {
      case None => Map(m.xs -> 1, m.os -> 1)
      case Some(X) => Map(m.xs -> 3, m.os -> 0)
      case Some(O) => Map(m.os -> 3, m.xs -> 0)
    }
  }

  case class Position(row: Int, col: Int)
  case class Board(arr: Array[Option[XO]]) {
    def at(p: Position): Option[XO] = arr(3*p.row + p.col)
    def place(sym: XO, p: Position): Board = {
      if(at(p).isDefined) throw new IllegalArgumentException(s"$p is not empty")
      else this.copy(
        arr = arr.updated(3*p.row + p.col, Some(sym))
      )
    }
    def winner: Option[XO] = {
      Stream(
        for { a <- arr(0); b <- arr(1); c <- arr(2); if a == b && b == c } yield a,
        for { a <- arr(3); b <- arr(4); c <- arr(5); if a == b && b == c } yield a,
        for { a <- arr(6); b <- arr(7); c <- arr(8); if a == b && b == c } yield a,
        for { a <- arr(0); b <- arr(3); c <- arr(6); if a == b && b == c } yield a,
        for { a <- arr(1); b <- arr(4); c <- arr(7); if a == b && b == c } yield a,
        for { a <- arr(2); b <- arr(5); c <- arr(8); if a == b && b == c } yield a,
        for { a <- arr(0); b <- arr(4); c <- arr(8); if a == b && b == c } yield a,
        for { a <- arr(2); b <- arr(4); c <- arr(6); if a == b && b == c } yield a
      ).flatten.headOption
    }
    def isPlayable: Boolean = arr.exists(_.isEmpty) && winner.isEmpty
  }
  object Board {
    val empty = Board(Array.fill(9)(Option.empty))
    val spaces = Seq.tabulate(9) { case n => Position(n/3, n%3) }
  }
  case class Play(pos: Position, symbol: XO)

  sealed trait XO
  object XO {
    object X extends XO
    object O extends XO
    val all: Set[XO] = Set(X, O)
  }


  abstract class Player(val symbol: XO) {
    def play(s: State): Play
  }

  case class State(
    board: Board,
    plays: Seq[(Id[Player], Play)]) {
    def evolve(pl: Id[Player], p: Play): State = this.copy(
      board = board.place(p.symbol, p.pos),
      plays = plays :+ (pl, p)
    )
    def isPlayable: Boolean = board.isPlayable
  }
  object State {
    val empty = State(Board.empty, Seq.empty)
  }
  case class Match(
    id: Long,
    xs: Id[Player],
    os: Id[Player],
    state: State) {
    def currentPlayer = if (state.plays.length % 2 == 0) xs else os
  }
  object Match {
    def empty(id: Long, xs: Id[Player], os: Id[Player]) = Match(
      id = id,
      xs = xs,
      os = os,
      state = State.empty
    )
  }
}


