import patmat.Huffman._
val pairList: List[(Char, Int)] = List(('a', 1), ('b', 2), ('c', 4))
val charList: List[Char] = List('a', 'b', 'c', 'd', 'a', 'a', 'a', 'c', 'c')
val leafList: List[Leaf] = List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4), Leaf('z', 8), Leaf('g', 10), Leaf('h', 12))
val codeTree1: CodeTree = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val codeTree2: CodeTree = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
//val helloChars: List[Char] = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
val helloChars: List[Char] = "hello, world".toList
val loremString: String = "Lorem ipsum dolor sit amwet, cu his mutat ridens. Ea oratio omnesbque vix. Porro aliquam principes nzke sjed, te maiorum assentior eos. Dicam mediocrem at sea, quod doming ne sit. Ex eam facilisis patrioque vulputate, ornatus moderatius quo te. Ad mel aliquam voluptua scripserit."
val lorem: List[Char] = loremString.toLowerCase.filter(c => !List(' ', ',', '.', '\'').contains(c)).toList

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
makeOrderedLeafList(times(lorem))
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

val loremTree = createCodeTree(lorem)
val loremEncoder = encode(loremTree)_
val frenchEnoder = encode(frenchCode)_
lorem filter (x => x == 'c')
loremEncoder(List('k')).length
frenchEnoder(List('e')).length
def countLeaf(tree: CodeTree, p: (Char) => Boolean): Int = tree match {
  case Leaf(c, w) => if (p(c)) 1 else 0
  case Fork(l, r, _, _) => countLeaf(l, p) + countLeaf(r, p)
}

def countFork(tree: CodeTree, p: (List[Char]) => Boolean): Int = tree match {
  case Leaf(c, w) => 0
  case Fork(l, r, c, _) => if (p(c)) countFork(l, p) + countFork(r, p) + 1 else countFork(l, p) + countFork(r, p) + 0;
}

countLeaf(frenchCode, x => true)
countFork(frenchCode, x => true)
lorem map (c => countLeaf(loremTree, x => x == c)) filter(n => n != 1)
def makeSource(tree: CodeTree): String =  tree match {
  case Leaf(c, w) => c.toString * w
  case Fork(l, r, _, _) => makeSource(l) + makeSource(r)
}

val myfrenchTree = createCodeTree(makeSource(frenchCode).toList)
countLeaf(myfrenchTree, x => true)
countFork(myfrenchTree, x => true)
val c = 'z'
countFork(myfrenchTree, x => x.contains(c))
countFork(frenchCode, x => x.contains(c))
val myfrenchEnoder = encode(myfrenchTree)_
val cl = List(c)
myfrenchEnoder(cl).length
frenchEnoder(cl).length

def insert(element: CodeTree, sortedTrees: List[CodeTree]): List[CodeTree] = {
  val (lt, gt) = sortedTrees span(x => weight(x) < weight(element))
  lt ::: (element :: gt)
}

val listCodeTree = makeOrderedLeafList(times(helloChars))
insert(Leaf('r', 2), listCodeTree)

