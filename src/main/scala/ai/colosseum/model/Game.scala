package ai.colosseum.model

trait Game {
  type P
  type M
  def play(ps: Map[Id[P], P]): M
  def score(m: M): Map[Id[P], Int]
  def simulate(ps: Map[Id[P], P]): Map[Id[P], Int] = score(play(ps))
}
