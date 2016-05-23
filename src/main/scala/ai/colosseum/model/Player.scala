package ai.colosseum.model

import ai.colosseum.game.Game

trait Player[G <: Game] {

}

case class PlayerInstance[G <: Game](
  id: Id[PlayerInstance]) {

}

trait Play[G <: Game] {
  def player: Id[PlayerInstance]
}
