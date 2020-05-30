class hw5 {

  def list_add(l1: List[Int], l2: List[Int]): List[Int] = {
    //두 개의 리스트를 원소 단위로 더하여 하나의 리스트로 반환하는 함수 list_add을 작성하세요. (단, 리스트의 끝에서부터 더합니다. 둘 중 하나의 리스트가 더 길 땐 남는 값을 그대로 출력합니다.)

    l2.reverse.zipAll(l1.reverse, 0, 0).collect{case (x, y) => x+y}

  }
  println("** p1 **")
  println(list_add(List(1, 2, 3), List(1, 2, 3, 4, 5)))

  def insort(l: List[Int], m: Int): List[Int] = {
    // 리스트에 정수형 숫자 m 를 추가하고 정렬하여 반환하는 함수 insort를 작성하세요.
    l :+ m

  }
  println("** p2 **")
  println(insort(List(1, 2, 4, 5), 3))

  def ltake(l: List[Int], i: Int): List[Int] = {
    //리스트의 끝에서부터 i 개의 원소를 포함하는 리스트를 반환하는 함수 ltake를 작성하세요.
    l.drop(l.length-i)

  }
  println("** p3 **")
  println(ltake(List(1, 2, 3, 4), 2))

  def lall(f: Int => Boolean, l: List[Int]): Boolean = {
    //정수 리스트 l과 정수를 입력받아 불 값을 출력하는 함수 f를 입력받아 리스트의 모든 원소가 f를 적용했을 때 true로 평가되면 true, 아니면 false를 반환하는 함수 lall를 작성하세요.

//    l.foldLeft(true)((x: Boolean ,y: Int) =>x || f)
    l.filter(f)==l

  }
  println("** p4 **")
  println(lall(x => x > 0, List(1, 2, 3, 4)))

  def lmap(f: Int => Int, l: List[Int]): List[Int] = {
    //리스트의 모든 원소에 함수 f를 적용한 결과값들의 리스트를 반환하는 함수 lmap을 작성하세요.
    l.map(f)

  }
  println("** p5 **")
  println(lmap(x => x + 1, List(1, 2, 3)))

  def lfilter(f:Int => Boolean, l: List[Int]): List[Int] = {
    //리스트의 모든 원소에 함수 f를 적용하고 결과값이 true인 값들만 모아 리스트로 반환하는 함수 lfilter를 작성하세요.
    l.filter(f)
  }
  println("** p6 **")
  println(lfilter(x => x > 2, List(1, 2, 3)))

  def ltabulate(n: Int, f: Int => Int): List[Int] = {
    (0 to n-1).toList.map(f).reverse
  }
  println("** p7 **")
  println(ltabulate(4, (x:Int) => x * x))

  def lrev(l: List[Int]): List[Int] = {
    l.reverse
  }
  println("** p8 **")
  println(lrev(List(1, 2, 3)))

  def lconcat(l: List[List[Int]]): List[Int] = {
    l.flatten

  }
  println("** p9 **")
  println(lconcat(List(List(1, 2, 3), List(4, 5, 6))))

  def lfoldr(f: (Int, Int) => Int, l: List[Int], e: Int): Int = {
    l.foldRight(e)(f)
  }
  println("** p10 **")
  println(lfoldr((x:Int, y:Int) => x - y, List(1, 2, 3), 0))

  def lzip(a: List[String], b: List[Int]): List[(String, Int)] = {
    if(a.length > b.length){
      a.drop(a.length-b.length).zipAll(b, "a", 0)
    }else{
      a.zipAll(b.drop(b.length-a.length), "a", 0)
    }
  }
  println("** p11 **")
  println(lzip(List("A", "B", "C", "D"), List(1, 2, 3, 4, 5, 6)))

  def split(l: List[Int]): (List[Int], List[Int]) = {
    l.partition(_% 2 == 1)
  }
  println("** p12 **")
  println(split(List(1, 2, 3, 4, 5)))

  def cartprod(l1: List[Int], l2: List[Int]): List[(Int, Int)] = {
    l1.flatMap(p => l2.map(o => (p, o)))
  }
  println("** p13 **")
  println(cartprod(List(1, 2), List(3, 4, 5)))


}

object hw5_test{
  def main(args: Array[String]): Unit ={
    var p = new hw5
  }
}
