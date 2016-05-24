package ai.colosseum.game.tictactoe

import ai.colosseum.game.tictactoe.TicTacToe._
import ai.colosseum.model.Id
import ai.colosseum.player.tictactoe.UpperLeftPlayer
import org.specs2.mutable.Specification

class TicTacToeTest extends Specification {
  "TicTacToe" should {
    "have a trivial player" in {
      val px = new UpperLeftPlayer(X)
      val s = TicTacToe.State.empty
      px.play(s) === Play(Position(0,0), X)
    }
    "observe when a play is illegal" in {
      val s = TicTacToe.State.empty
      s.evolve(Id(1), Play(Position(0,0), X))
        .evolve(Id(1), Play(Position(0,0), X)) must throwA(new IllegalArgumentException("Position(0,0) is not empty"))
    }
    "carry out a game" in {
      val m = TicTacToe.play(px = new UpperLeftPlayer(X), po = new UpperLeftPlayer(O))
      m.state.board.winner must beSome(X)
      m.state.plays.length === 7
    }
  }
}
