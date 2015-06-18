package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestData {
    val l1: List[Char] = List('a', 'b', 'a', 'c', 'd', 'a')
    val codeTree1: CodeTree = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val codeTree2: CodeTree = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val codeTree3: CodeTree = Fork(Leaf('a', 8), Fork(Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), Fork(Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2), Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'), 2), List('e', 'f', 'g', 'h'), 4), List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9), List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 17)
    val leafList1: List[Leaf] = List(Leaf('a', 1))
    val leafList2: List[Leaf] = List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4), Leaf('z', 8), Leaf('g', 10), Leaf('h', 12))
    val leaflist3 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
  }

  test("weight of a larger tree") {
    new TestData {
      assert(weight(codeTree1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestData {
      assert(chars(codeTree2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times 1") {
    new TestData {
      assert(times(l1).exists {
        case ('a', 3) => true
        case _ => false
      })
      assert(times(l1).size === 4)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    new TestData {
      assert(singleton(leafList1))
      assert(singleton(leafList2) === false)
    }
  }

  test("combine of some leaf list") {
    new TestData {
      assert(combine(leaflist3) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
      assert(combine(makeOrderedLeafList(times(l1))) === List(Fork(Leaf('b', 1), Leaf('c', 1), List('b', 'c'), 2), Leaf('d', 1), Leaf('a', 3)))
      assert(combine(leafList1) === leafList1)
      assert(combine(Nil) === Nil)
    }
  }

  test("decode") {
    new TestData {
//      assert(decode(codeTree3, List[Bit](1, 0, 1, 1)) === List('d'))
      assert(decode(codeTree3, List[Bit](1, 0, 1, 1, 1, 0, 1, 1)) === List('d', 'd'))
    }
  }

  ignore("decode and encode a very short text should be identity") {
    new TestData {
      assert(decode(codeTree1, encode(codeTree1)("ab".toList)) === "ab".toList)
    }
  }
}
