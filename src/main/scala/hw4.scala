import com.sun.org.apache.xpath.internal.operations.Bool

class hw4 {

  abstract class Tree[Int]
  case class Leaf[Int](elem: Int) extends Tree[Int]
  case class Node[Int](elem: Int, left: Tree[Int], right: Tree[Int]) extends Tree[Int]

  val tree = Node(7, Node(3, Leaf(1), Leaf (2)), Leaf(4))

  abstract class Exp[Int]
  case class INT[Int](x: Int) extends Exp[Int]
  case class ADD[Int](x: Exp[Int], y: Exp[Int]) extends Exp[Int]
  case class SUB[Int](x: Exp[Int], y: Exp[Int]) extends Exp[Int]
  case class MUL[Int](x: Exp[Int], y: Exp[Int]) extends Exp[Int]
  case class DIV[Int](x: Exp[Int], y: Exp[Int]) extends Exp[Int]
  case class REM[Int](x: Exp[Int], y: Exp[Int]) extends Exp[Int]

  val exp = ADD(SUB(INT(100), INT(10)), MUL(INT(2), INT(8)))

  abstract class Fma[Boolean]
  case object True extends Fma[Boolean]
  case object False extends Fma[Boolean]
  case class Neg[Boolean](x: Fma[Boolean]) extends Fma[Boolean]
  case class Or[Boolean](x: Fma[Boolean], y: Fma[Boolean]) extends Fma[Boolean]
  case class And[Boolean](x: Fma[Boolean], y: Fma[Boolean]) extends Fma[Boolean]
  case class Imply[Boolean](x: Fma[Boolean], y: Fma[Boolean]) extends Fma[Boolean]

  val fma = Imply(And(True, Or(True, False)), False)

  def sum_tree( t: Tree[Int]):  Int = t match {

    //재귀함수를 이용하여 트리의 합을 구하세요
    case Leaf(elem) => elem
    case Node(elem,left,right) => elem + sum_tree(left) + sum_tree(right)

  }
  println("** p6 **")
  println(sum_tree(tree))

  def depth(t: Tree[Int]): Int = t match {
    //재귀함수를 이용하여 트리의 깊이를 구하세요

    case Leaf(elem) => 0
    case Node(elem,left,right) => 1 + depth(left)

  }
  println("** p7 **")
  println(depth(tree))

  def bin_search(t: Tree[Int], x: Int):Boolean = t match  {
    //재귀함수를 이용하여 트리의 바이너리 3 을 찾아보세요
    case Leaf(elem) => elem==x
    case Node(elem,left,right) => elem==x  || bin_search(left, x) || bin_search(right, x)

  }
  println("** p8 **")
  println(bin_search(tree, 3))

      def interp(exp: Exp[Int]): Int = exp match {
        // 재귀함수를 사용하여 곱하기,더하기,뺴기,나누기,나머지 를 계산하는 함수를 만드세요
    case INT(x) => x
    case ADD(x, y) => interp(x) + interp(y)
    case SUB(x, y) => interp(x) - interp(y)
    case MUL(x, y) => interp(x) * interp(y)
    case DIV(x, y) => interp(x) / interp(y)
    case REM(x, y) => interp(x) % interp(y)

  }
  println("** p9 **")
  println(interp(exp))


  def formula(fma: Fma[Boolean]): Boolean = fma match {
    // 재귀함수를 사용하여 Neg,or,And,Implies 를 계산하는 함수를 만드세요

    case True => true
    case False => false
    case Neg(x) => !formula(x)
    case Or(x, y) => formula(x) || formula(y)
    case And(x, y) => formula(x) && formula(y)
    case Imply(x, y) => if(formula(x) && !formula(y)){false}else{true}
  }
  println("** p10 **")
  println(formula(fma))

}




object hw4_test{
  def main(args: Array[String]): Unit ={
    var p = new hw4

  }
}
