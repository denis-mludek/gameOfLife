package gameOfLife

object Rules {

  trait Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean
  }

  object Replicator extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = nbAliveNeighboors match {
      case 1 | 3 | 5 | 7 => true
      case _ => false
    }
  }

  object Seeds extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(!alive){
        nbAliveNeighboors match {
          case 2 => true
          case _ => false
        }
      } else true
    }
  }

  object LifeWithoutDeath extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(alive){
        nbAliveNeighboors match {
          case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 3 => true
          case _ => false
        }
      }
    }
  }

  object Life34 extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = nbAliveNeighboors match {
      case 3 | 4 => true
      case _ => false
    }
  }

  object Diamoeba extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(alive){
        nbAliveNeighboors match {
          case 5 | 6 | 7 | 8 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 3 | 5 | 6 | 7 | 8 => true
          case _ => false
        }
      }
    }
  }

  object TwoXTwo extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(alive){
        nbAliveNeighboors match {
          case 1 | 2 | 5 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 3 | 6 => true
          case _ => false
        }
      }
    }
  }

  object HighLife extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(alive){
        nbAliveNeighboors match {
          case 2 | 3 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 3 | 6 => true
          case _ => false
        }
      }
    }
  }

  object DayNNight extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(alive){
        nbAliveNeighboors match {
          case 3 | 4 | 6 | 7 | 8 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 3 | 6 | 7 | 8 => true
          case _ => false
        }
      }
    }
  }
  object Morley extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if(alive){
        nbAliveNeighboors match {
          case 2 | 4 | 5 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 3 | 6 | 8 => true
          case _ => false
        }
      }
    }
  }
  object Anneal extends Rules {
    def apply(alive: Boolean, nbAliveNeighboors: Int): Boolean = {
      if (alive) {
        nbAliveNeighboors match {
          case 3 | 5 | 6 | 7 | 8 => true
          case _ => false
        }
      } else {
        nbAliveNeighboors match {
          case 4 | 6 | 7 | 8 => true
          case _ => false
        }
      }
    }
  }
}
