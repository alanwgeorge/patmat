import patmat.Huffman._
val pairList: List[(Char, Int)] = List(('a', 1), ('b', 2), ('c', 4))
val charList: List[Char] = List('a', 'b', 'c', 'd', 'a', 'a', 'a', 'c', 'c')
val leafList: List[Leaf] = List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4), Leaf('z', 8), Leaf('g', 10), Leaf('h', 12))
val codeTree1: CodeTree = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val codeTree2: CodeTree = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
val helloChars: List[Char] = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')

def condition(pair: (Char, Int)): Boolean = {
  pair match {
    case ('z', n) => true
    case _ => false
  }
}
pairList.exists(condition)
pairList.exists {
  case ('z', n) => true
  case _ => false
}
def recurseIncrement(Target: Char, list: List[(Char, Int)]): List[(Char, Int)] = {
  list match {
    case (Target, n) :: xs => println("bingo"); (Target, n + 1) :: recurseIncrement(Target, xs)
    case x :: xs =>
      if (x._1 == Target) (Target, x._2 + 1) :: recurseIncrement(Target, xs)
      else x :: recurseIncrement(Target, xs)
    case Nil => Nil
  }
}
recurseIncrement('c', pairList)
recurseIncrement('a', pairList)
recurseIncrement('a', Nil)
times(charList)
makeOrderedLeafList(times(charList))
singleton(leafList)
singleton(List(Leaf('a', 1)))
def u = until(singleton, combine)_
u(List(codeTree1, codeTree2))
createCodeTree(helloChars)
decode(frenchCode, secret)
val table1: CodeTable = List(('a', List(1,1,1)), ('b', List(1,0,1)))
def test(table: CodeTable, C: Char): (Char, List[Bit]) = table match {
  case (C, b) :: xs => println(b); ('a', Nil)
  case x :: xs => println(x); test(xs, C)
  case _ => ('0', Nil)
}

test(table1, 'a')

