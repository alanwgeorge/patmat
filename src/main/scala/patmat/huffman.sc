import patmat._

val pairList: List[(Char, Int)] = List(('a', 1), ('b', 2), ('c', 4))
val charList: List[Char] = List('a', 'b', 'c', 'd', 'a', 'a', 'a', 'c', 'c')

def condition(pair: (Char, Int)): Boolean = {
  pair match {
    case ('z', n) => true
    case _ => false
  }
}

pairList.exists(condition)

def recurseIncrement(target: Char, list: List[(Char, Int)]): List[(Char, Int)] = {
//  println("target = " + target)
//  println("list = " + list)
  list match {
//    case (target, n) :: y => {
//      println("case 1")
//      (target, n + 1) :: recurseIncrement(target, y)
//    }
    case x :: xs =>  {
//      println("case 2")
      if (x._1 == target) (target, x._2 + 1) :: recurseIncrement(target, xs)
      else x :: recurseIncrement(target, xs)
    }
    case Nil => {
//      println("case 3")
      Nil
    }
  }
}
recurseIncrement('c', pairList)
recurseIncrement('a', pairList)
recurseIncrement('a', Nil)
Huffman.times(charList)
