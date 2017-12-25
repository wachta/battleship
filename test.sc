val x = 10
val string1 = "Hello World"
string1.length
string1.replace("H","G")
string1

def sum1(x:Int,y:Int):Int = {
  x+y
}
sum1(3,6)

def squareArea(a:Int):Int = {
  a*a
}
squareArea(4)

def division1(a:Double,b:Double) = {
  if (b != 0) a/b
  else -1.0
}

division1(4,2)

def loop (i:Int,max:Int):String ={
  if(i<max){
    loop(i+1,max)
  }
  else {"I REACHED 10"}
}

println(loop(4,10))
println(loop(8,10))

// Data Structures
val arr = Array(1,2,3,4,5,6,7,8,9,10)
arr(1)

def readArr (a:Array[Int],start:Int):String = {
  if(start>a.length) "Too Big"
  else{
    a(start).toString
    readArr(a,start+1)
  }
}

println(readArr(arr,300))

//Why that crashed ? We will never know

def headler (a:List[Any]):Any = {
  a.head
}

val l = List(4,5,3,1,4,4,3,3,2,3)
headler(l)


val z = Array(1,2,4)

def recSumMap(a: List[Int], sumHolder: Int = 0): Int = {
  if(a.nonEmpty)recSumMap(a.tail,a.head + sumHolder)
  else sumHolder
}

recSumMap(l)


def getElem(list: List[Any],index:Int): Any={
  index match{
    case 0 => list.head
    case _=> getElem(list.tail,index-1)
  }
}

getElem(l,0)

def ship(length:Int,startX:Int,startY:Int,direction:String):List[(Int,Int)] = {
  direction match {
    case "horizontal" => {
      require(startX+length<8)
      ((startX.to(startX+length)).toList) zip (List.fill(length)(startY))
    }
    case "vertical" => {
      require(startY+length<8)
      ((startY.to(startY+length)).toList) zip (List.fill(length)(startX))
    }
  }
}

def shoot(shot:(Int,Int),ship:List[(Int,Int)]):Boolean ={
  if(ship.isEmpty) 1==2
  else {
    if (ship.head == shot) 1==1
    else shoot(shot, ship.tail)
  }
}

val vessel = ship(4,1,5,"horizontal")

shoot((1,2),vessel)