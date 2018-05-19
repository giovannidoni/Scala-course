import patmat.Huffman._

val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

encode(t1)("ab".toList)

//def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
//
//  def run(subtree: CodeTree, subits: List[Bit], acc: List[Char]): List[Char] = subits match {
//
//    case Nil => subtree match {
//      case Leaf(char, _) => acc:+char
//      case Fork(_,_,_,_) => throw new Exception("Wrong encoding.")
//    }
//    case x::xs => subtree match {
//      case Leaf(char, _) => run(tree, subits, acc:+char)
//      case Fork(left, right, _, _) =>
//        if (x == 0) run(left, xs, acc)
//        else run(right, xs, acc)
//    }
//  }
//  run(tree, bits, List())
//}

decode(t1, encode(t1)("ab".toList)) == "ab".toList