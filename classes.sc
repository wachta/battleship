case class Ship(var name: String,var startX: Int,var length: Int, var startY: Int ,var direction: String,var battleSize:Int){
  val hitpoints = create(length,startX,startY,direction,battleSize)

  def create(length:Int,startX:Int,startY:Int,direction:String,battleSize:Int):List[(Int,Int)] = {
    direction match {
      case "horizontal" => {
        require(startX+length<=battleSize)
        ((startX.to(startX+length)).toList) zip (List.fill(length)(startY))
      }
      case "vertical" => {
        require(startY+length<=battleSize)
        ((startY.to(startY+length)).toList) zip (List.fill(length)(startX))
      }
    }
  }
}

def shoot(shot:(Int,Int),ship:List[(Int,Int)]):Unit ={
  if(ship.isEmpty) {
    println("You missed")
  }
  else {
    if (ship.head == shot) {
      println("You hit something")
    }
    else {
      shoot(shot, ship.tail)
    }
  }
}

case class Battlefield(var size: Int){
  val BattlePos = create(size)

  def create(size: Int, holder: Int = 1, list:List[(Int,Int)]= List((0,0))):List[(Int,Int)] ={
    require(size>0)
    if(size >= holder) create(size,holder+1,list ::: (((1.to(size)).toList) zip (List.fill(size)(holder))))
    else list
  }
}

val battlefield = new Battlefield(7)

val Cruiser = new Ship("ElHuerra",3,4,5,"horizontal",battlefield.size)
battlefield.BattlePos
battlefield.size
Cruiser.hitpoints

shoot((1,2),Cruiser.hitpoints)