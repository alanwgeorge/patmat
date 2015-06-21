package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestData {
    def a = Leaf('a', 8)
    def b = Leaf('b', 3)
    def c = Leaf('c', 1)
    def d = Leaf('d', 1)
    def e = Leaf('e', 1)
    def f = Leaf('f', 1)
    def g = Leaf('g', 1)
    def h = Leaf('h', 1)

    val l1: List[Char] = List('a', 'b', 'a', 'c', 'd', 'a')
    val l2: List[Char] = "Lorem ipsum dolor sit amet, cu his mutat ridens. Ea oratio omnesque vix. Porro aliquam principes ne sed, te maiorum assentior eos. Dicam mediocrem at sea, quod doming ne sit. Ex eam facilisis patrioque vulputate, ornatus moderatius quo te. Ad mel aliquam voluptua scripserit.".toList
    val codeTree1: CodeTree = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val codeTree2: CodeTree = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val codeTree3 = makeCodeTree(a, makeCodeTree(makeCodeTree(b, makeCodeTree(c, d)), makeCodeTree(makeCodeTree(e, f), makeCodeTree(g, h))))
    val leafList1: List[Leaf] = List(Leaf('a', 1))
    val leafList2: List[Leaf] = List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4), Leaf('z', 8), Leaf('g', 10), Leaf('h', 12))
    val leaflist3 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    val table1: CodeTable = List(('a', List(1, 1, 1)), ('b', List(1, 0, 1)))
  }

  test("weight of a larger tree") {
    new TestData {
      assert(weight(codeTree1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestData {
      assert(chars(codeTree2) === List('a', 'b', 'd'))
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
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
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
      assert(combine(makeOrderedLeafList(times(l1))) === List(Leaf('d', 1), Fork(Leaf('b', 1), Leaf('c', 1), List('b', 'c'), 2) , Leaf('a', 3)))
      assert(combine(leafList1) === leafList1)
      assert(combine(Nil) === Nil)
    }
  }

  test("createCodeTree") {
    new TestData {
      assert(weight(createCodeTree(l1)) === l1.length)
      assert(weight(createCodeTree(l2)) === l2.length)
    }
  }

  test("decode") {
    new TestData {
      assert(decode(codeTree3, List[Bit](1, 0, 1, 1)) === "d".toList)
      assert(decode(codeTree3, List[Bit](1, 0, 1, 1, 1, 0, 1, 1)) === "dd".toList)
      assert(decode(codeTree3, List[Bit](1, 0, 1, 1, 1, 0, 1, 1, 0)) === "dda".toList)
      assert(decode(codeTree3, List[Bit](1, 0, 0, 0, 1, 0, 1, 0)) === "bac".toList)
      assert(decode(frenchCode, secret) === "huffmanestcool".toList)
    }
  }

  test("decode and encode") {
    new TestData {
      val encode3 = encode(codeTree3) _
      assert(encode3(List('d')) === List(1, 0, 1, 1))
      assert(encode3(List('a')) === List(0))
      assert(encode3(List('a', 'd')) === List(0) ::: List(1, 0, 1, 1))
      assert(decode(codeTree1, encode(codeTree1)("ab".toList)) === "ab".toList)
      assert(encode(frenchCode)("huffmanestcool".toList) === secret)
      assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }


  test("codeBits CodeTable") {
    new TestData {
      val codeBits1 = codeBits(table1) _

      assert(codeBits1('a') === List(1, 1, 1))
      assert(codeBits1('b') === List(1, 0, 1))
    }
  }

  test("convert CodeTable") {
    new TestData {
      val codBits3 = codeBits(convert(codeTree3)) _

      assert(codBits3('a') === List(0))
      assert(codBits3('d') === List(1, 0, 1, 1))
    }
  }

  test("decode and quickEncode") {
    new TestData {
      val encode3 = quickEncode(codeTree3) _
      assert(encode3(List('d')) === List(1, 0, 1, 1))
      assert(encode3(List('a')) === List(0))
      assert(encode3(List('a', 'd')) === List(0) ::: List(1, 0, 1, 1))
      assert(decode(codeTree1, quickEncode(codeTree1)("ab".toList)) === "ab".toList)
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
      assert(decode(frenchCode, quickEncode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
}