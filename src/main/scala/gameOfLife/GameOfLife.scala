package gameOfLife

import doodle.core._
import UI._

object GameOfLife extends App {

  def start(size: Int) {
    val ticks: Events[Unit] = Events.every(500)

    val world: Events[World] = {
      ticks.fold(new World(0, Seq.empty[Cell], size))((tick, world) => {
        val w = world.nextWorld()
        val gen = w.genNumber
        println(s"World generation $gen")
        w
      })
    }

    val images: Events[Image] = {
      world.map(w => buildPlateau(w))
    }

    show(images)
  }

  start(30)
}
