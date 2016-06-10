package gameOfLife

import scala.util.Random

case class Coordinates(x: Int, y: Int)

case class Cell(loc: Coordinates) {

  def coordinatesNeighboors(widthPlateau: Int): Seq[Coordinates] = {
    for {
      x <- this.loc.x-1 to this.loc.x+1
      y <- this.loc.y-1 to this.loc.y+1
      if !(x == this.loc.x && y == this.loc.y)
    } yield Coordinates(
      safeCoordinates(x, widthPlateau),
      safeCoordinates(y, widthPlateau)
    )
  }

  def safeCoordinates(n: Int, widthPlateau: Int): Int = {
    ((n % widthPlateau) + widthPlateau) % widthPlateau
  }
}

case class World(genNumber: Int, aliveCells: Seq[Cell], worldSize: Int) {

  def isCellAlive(coord: Coordinates): Boolean = {
    aliveCells.exists(c => c.equals(Cell(coord)))
  }

  def nextWorld(): World = {
    genNumber match {
      case 0 => {
        val cells =
          for {
            x <- 1 to worldSize
            y <- 1 to worldSize
            if (Random.nextDouble() <= .5)
          } yield Cell(Coordinates(x, y))
        new World(genNumber + 1, cells, worldSize)
      }
      case x if x > 0 => {
        val allCoordinates =
          for {
            x <- 1 to worldSize
            y <- 1 to worldSize
          } yield Coordinates(x, y)

        val cells =
          allCoordinates.filter(coord => {
            val nbAliveNeighboors =
              Cell(coord).coordinatesNeighboors(worldSize)
                .map { neighboor => (neighboor, isCellAlive(neighboor)) }
                .count { case (coord, alive) => alive }

            if (isCellAlive(coord)) nbAliveNeighboors == 2 || nbAliveNeighboors == 3
            else nbAliveNeighboors == 3
          }).map(coord => Cell(coord))

        new World(genNumber + 1, cells, worldSize)
      }
    }
  }
}
