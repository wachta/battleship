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

def findElemPos (elem1 : Position, elem2 : List[Position]): Position = { //Wenn mit ergebnis in diesem Fall Position weitergearbeitet werden wird MUSS DER Return Type Pos sein deswegen ähnliche funks.
  elem2.length match{
    case 0 => Position(0,0)
    case otherwhise => {
      if (elem1 == elem2.head) elem2.head
      else findElemPos(elem1,elem2.tail)
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

def getElemListPos(list: List[List[Position]],index:Int): List[Position] ={ //Es muss dasverwendet werden sonst knallts
  index match{
    case 0 => list.head
    case _=> getElemListPos(list.tail,index-1)
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

case class fleet (ships : List[List[Position]]) {
  var shipsPos = ships
  //überschreibt unsere schiffsPositionen und nimmt die Position heraus kann damit als HitPointListe verwendet werden
  def removeHit (Pos : Position) : Unit = { //Funktion entfernt Treffer aus gegebener List Liste von Positionen
    shipsPos = shipsPos.map(x => x.filter(_ != Pos))
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

case class player (id : Int, name : String) {

  var shots = List(Position(0, 0))
  var takenshots = 0 //besser als length von shots zu nehmen

  def getShot(turn: Int) = { //function fürn slider
    getElem(this.shots, turn)
  }

  def shoot (shotPos: Position, flotte : fleet) : Unit = {
    require(shotPos != Position(0,0),"Diese Koordinate ist reserviert wird für Fehlerbugging verwendet gibt es im Spiel nicht")
    if(existIn(shotPos,this.shots)) println("Auf Koordinate " + shotPos + " wurde bereits gefeuert.") //funktion terminiert
    else { //Treffer existiert noch nicht wir durchsuchen die Liste
      var i = 0
      while(i < flotte.ships.length) { //auch wenn getroffen sucht weiter
        var PosList = getElemListPos(flotte.shipsPos,i)
        if(findElemPos(shotPos,PosList) != Position(0,0)){
          println("Das Schiff " + i + " wurde an der Koordinate " + shotPos + " getroffen." + PosList.length)
         if(PosList.length == 1) println("Ein Schiff wurde zerstört !") //Wenn zum Zeitpunkt des Treffers die länge der betroffenen Liste eins ist ist danach das Schiff zerstört ;)
          flotte.removeHit(shotPos) //Wenn Treffer wird Koordinate entfernt
        }
        i += 1
      }
      takenshots += 1 //egal ob getroffen oder nicht takenshots wird um 1 erhöht
      shots = shots ::: List(shotPos) //der Schuss wird der Liste angefügt
    }
  }
}
//CLASSES END

//OLD CODE
/*def shoot(shotPos: Position, list: List[List[Position]]): Unit = {
    require(shotPos != Position(0, 0))
    var i = 0
    if (existIn(shotPos, shots)) println("Dorthin wurde bereits geschossen")
    else {
      while (i < list.length) { //Geht durch alle Listen von jedem schiff durch und schaut obs trifft
        shootsingle(shotPos, getElemListPos(list, i))
        i = i + 1
      }
      takenshots += 1
      shots = shots ::: List(shotPos)
    }
  }

  def shootsingle(shotPos: Position, list: List[Position]): Position = { //Angegebene Position Liste  is nocht net passend ...#
    if(list.length == 0) Position(0,0)
    else {
      if (list.length != 0 && list.head == shotPos) shotPos
      else (shootsingle(shotPos, list.tail))
    }
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
  */