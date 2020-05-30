class hw3 {
  var sum = 0
  var fac = 1

  def sum(n: Int): Int = {
    // 자기호출 함수를 사용하여 계산합니다.
    if(n!=0){
      sum = sum + n
      sum(n-1)
    }
    sum
  }



  def fac(n: Int): Int = {
    // n이 1이면 1로 출력합니다
    // 그렇지 않으면 재귀함수를 사용하여 계산합니다.
    if(n!=0){
      fac = fac * n
      fac(n-1)
    }
    fac
  }

  def fib(n: Int): Int = n match {
    // n 이 1 또는 2면 1을 출력합니다.
    // n이 3 이상인 경우에는
    case 1 => 1
    case 2 => 1
    case n => fib(n-1)+fib(n-2)

  }

  def gcd(n: Int, m: Int): Int = {
    // 작은 값과 큰 값을 먼저 출력합니다.
    // 유클리드 호제법을 사용하여 자기호출 함수로 계산하세요
    if(n<m){
      println("gcd " + n + " " + m)
    }else{
      println("gcd " + m + " " + n)
    }
    if(m==0){
      n
    }else{
      gcd(m,n%m)
    }

  }

  def max(list: List[Int]): Int = {
    // 정수 리스트에서 가장 큰 값을 반환하는 자기호출 함수를 작성하세요.
    // 빈 리스트를 입력받으면 0을 반환하세요.
    if (list.isEmpty) {
      0
    } else {
      var maxval = max(list.tail)
      if (maxval >= list.head)
        maxval
      else
        list.head
    }
  }
}

object hw3_test{
  def main(args: Array[String]): Unit = {
    var p = new hw3

    println("** p1 **")
    println(p.sum(100))

    println("** p2 **")
    println(p.fac(9))

    println("** p3 **")
    println(p.fib(10))

    println("** p4 **")
    println(p.gcd(15,20))

    println("** p5 **")
    var y = List(3, 4, 14, 7, 2)
    println(p.max(y))
  }
}