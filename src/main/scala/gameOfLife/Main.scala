package gameOfLife

import UI._

object Main extends App {

  def start(size: Int) {
    val ticks: Events[Unit] = Events.every(500)

    val world: Events[World] = {
      ticks.fold(new World(0, Seq.empty[Cell], size, Rules.TwoXTwo, .25))((tick, world) => {
        val w = world.next()
        val gen = w.genNumber
        println(s"World generation $gen")
        w
      })
    }

    show(images(world))
  }

  start(30)
}
