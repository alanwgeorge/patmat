def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (x, Nil) => x
      case (Nil, y) => y
      case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }
    val (first, second) = xs splitAt n
    merge(msort(first), msort(second))
  }
}

val l1 = List(3,2,-5,3,2,6,7,8,9)
val l2 = List("apple", "orange", "pinepple", "banana")

msort(l1)

def squareList1(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList1(ys)
}

squareList1(l1)

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)
squareList2(l1)

l1 map (x => x * x)
l1 filter (x => x > 0)
l1 filterNot (x => x > 0)
l1 partition (x => x > 0)

l1 takeWhile (x => x > 0)
l1 dropWhile (x => x > 0)
l1 span (x => x > 0)

val packTarget: List[String] = List("a", "a", "a", "b", "c", "c", "a")
val packResult: List[List[String]] = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys => val (first, rest) = xs span (x => x == y)
    first :: pack(rest)
}

pack(packTarget)

assert(pack(packTarget) == packResult)

def encode[T](list: List[T]): List[(T, Int)] = {
  val packed: List[List[T]] = pack(list)

  def encodePacked(packedTodo: List[List[T]]): List[(T, Int)] = {
    packedTodo match {
      case Nil => Nil
      case x :: xs => (x.head, x.length) :: encodePacked(xs)
    }
  }
  encodePacked(packed)
}

encode(packTarget)

def encode2[T](list: List[T]): List[(T, Int)] = {
  pack(list) map (xs => (xs.head, xs.length))
}

encode2(packTarget)

def f(x: Int) = if(x > 2) Some(x) else None
def g(x: Int) = List(x-1, x, x+1)

val list: List[Int] = List(1,2,3,4,5)

list map(x => f(x))
list map(x => g(x))
list flatMap (x => f(x))
list flatMap (x => g(x))
scala.collection.immutable.Map
val m: Map[Int, Int] = Map(1 -> 2, 2 -> 4, 3 -> 6)
m mapValues (x => f(x))
m mapValues (x => g(x))
m flatMap (x => f(x._2))
m flatMap (x => g(x._2))
