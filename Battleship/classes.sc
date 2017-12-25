//HELPING FUNCTIONS
def existIn (elem1 : Any, elem2 : List[Any]): Boolean = {
  elem2.length match{
    case 0 => "Dominik" == "isanicerdude"
    case otherwhise => {
      if (elem1 == elem2.head) 1==1
      else existIn(elem1,elem2.tail)
    }
  }
}

def hasSimiliarElements(list1 : List[Any], list2 : List[Any]): Boolean = {
  list1.length match {
    case 0 => 5==6
    case otherwhise => {
      if(existIn(list1.head,list2)) "gg" == "gg"
      else(hasSimiliarElements(list1.tail,list2))
      }
  }
}

def getElem(list: List[Any],index:Int): Any={
  index match{
    case 0 => list.head
    case _=> getElem(list.tail,index-1)
  }
}
//HELPING END

//CLASSES
case class Position (var x: Int,var y:Int ) {
  def getX = this.x
  def getY = this.y
}

case class battlefield (var size :Int) {
  val fields = construct(size)

  def construct (size : Int, sizeHolder : Int = size, holder : List[Position] =List(Position(0,0))):List[Position] = {
   require(sizeHolder > 0, "warum bloß ? I bin a zaubara.")
   size match {
      case 0 => holder.filter(_ != Position(0,0))
      case otherwise =>
        var i = 0
        var list = List(Position(0,0))
        while (i < sizeHolder) {
           list = list ::: List(Position(size,i+1))
           i=i+1
        }
        construct(size-1,sizeHolder,holder ::: list)
    }
  }
}

case class ship (length : Int , startPos : Position , direction : String) {
  val BattlePos = create(length, startPos,  direction)
  val hits = List(Position(0,0))
  require(length>0, "schnaml a schiff gsegn was gleich 0 oda kürza is ?? Na glab nit")

  def create(length : Int , startPos : Position , direction : String):List[Position] ={
    var list = List(startPos)
    var i = length-1
    direction match {
      case "horizontal" =>
        while(i > 0) {
          list = list ::: List(Position(startPos.getX + i,startPos.getY))
          i = i - 1
        }
        list
      case "vertical" =>
        while(i > 0) {
          list = list ::: List(Position(startPos.getX,startPos.getY + i))
          i = i -1
        }
        list
    }
  }
}

case class player (id : Int, name : String){

  var shots = List(Position(0,0))
  var takenshots = 0 //besser als length von shots zu nehmen

  def getShot (turn : Int) ={ //function fürn slider
    getElem(this.shots,turn)
  }

  def shoot (shotPos : Position,list : List[Position]): Unit = { //Angegebene Position Liste  is nocht net passend ...
    require(shotPos != Position(0,0))
    list.length match {
      case 0 => {
        println("daneben du pfeifn")
        shots = shots ::: List(shotPos) // FÜRN COUNTER LISTE SHOTS hat gleich viel einträge wie takenshots -> slider
        takenshots += 1
      }
      case otherwise =>
        if(existIn(shotPos,shots)) {
          println("Da hast schn hinballert du irrer")
        }
        else{
          if(shotPos == list.head) {
            println("Treffer bei " + shotPos)
            takenshots += 1
            shots = shots ::: List(shotPos)
          }
          else (shoot(shotPos,list.tail))
        }
    }
  }
}
//CLASSES END



//BELOW TESTING AND SAMPLE RUNS (USELESS SHIT)
//DELETE AFTER DONE
val Bismark = ship(5,Position(3,3),"vertical")
val Seenot = ship (3,Position(1,2),"horizontal")
val uboot = ship(1,Position(5,5),"vertical")

var Flotte = List(Bismark.BattlePos,Seenot.BattlePos,uboot.BattlePos)
getElem(Flotte,1)