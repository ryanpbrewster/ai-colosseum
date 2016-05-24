package ai.colosseum.player.tictactoe

import ai.colosseum.game.tictactoe.TicTacToe._

class UpperLeftPlayer(symbol: XO) extends Player(symbol) {
  def play(s: State): Play = {
    val legalMove = Board.spaces.find { s.board.at(_).isEmpty }
    Play(
      legalMove.getOrElse(throw new IllegalStateException("No legal moves")),
      symbol = symbol
    )
  }
}
