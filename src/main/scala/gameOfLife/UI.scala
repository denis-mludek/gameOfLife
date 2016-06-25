package gameOfLife

import javax.swing.JFrame

import doodle.core.{Circle, Color, Image, Rectangle}
import doodle.jvm.DoodlePanel

object UI {

  /* *
   * Generic functions to build images from doodle lib.
   * */

  trait Show[-A] {
    def image(a: A): Image
  }

  def layout(op: (Image, Image) => Image, image: Int => Image, n: Int): Image = {
    if(n == 1) image(n)
    else op(image(n), layout(op, image, n-1))
  }

  def stack(image: Int => Image, count: Int) =
    layout((img1, img2) => img1 on img2, image, count)

  def show(events: Events[Image]): Unit = {
    val panel = DoodlePanel(Circle(0) lineWidth 0)
    events.foreach(image => panel.image = image)
    val frame = new JFrame("Game of life")
    frame.getContentPane.add(panel)
    frame.pack()
    frame.setVisible(true)
  }

  /* *
  * Game of Life
  * */

  def buildPlateau(world: World): Image = {
    val cellDead = Rectangle(world.worldSize, world.worldSize)
    val cellAlive = Rectangle(world.worldSize, world.worldSize).fillColor(Color.darkCyan)

    def lineImage(y: Int): Image = {
      layout((img1, img2) => img1 beside img2, x => if(world.isCellAlive(Coordinates(x, y))) cellAlive else cellDead, world.worldSize)
    }

    val lines = layout((img1, img2) => img1 above img2, y => lineImage(y), world.worldSize)
    lines
  }

  implicit val worldShow = new Show[World] {
    override def image(a: World): Image =
      buildPlateau(a)
  }

  def images(as: Events[World])(implicit show: Show[World]): Events[Image] =
    as.map(w => show.image(w))
}
