class hw2 {
  def sum(n: Int): Int = {
    if (n <= 0) {
      n - n
    } else {
      n * (n + 1) / 2
    }
  }

    def circle(r: Double): Double = {

      if(r<=0.0){
        r-r
      }else{
        3.14*r*r
      }
    }
  def concat(name: String): String = {
    name
  }

  def xor(x: Boolean, y: Boolean) : Boolean = {
    x ^ y
  }

  def triangle(x: Int, y:Int, z:Int) : Boolean = {
    // 세 변 중에 하나라도 0보다 작거나 같으면 삼각형은 존재하지 않습니다.
    // 세 변이 모두 0보다 크면 위의 조건을 사용하여 삼각형이 존재하는지 확인해줍니다.
    var long = 0
    var mid = 0
    var short = 0
    long = x
    if(long<y){
      if(y<z){
        long = z
        mid = y
        short = x
      }else if(y>z){
        long = y
        if(x<z){
          mid = z
          short = x
        }else if(x>z){
          mid = x
          short = z
        }
      }
    }else if(long>y){
      if(y>z){
        mid = y
        short = z
      }else if(y<z){
        mid = z
        short = y
      }
    }

    if(x<=0 | y<=0 | z<=0){
      false
    }else{
      (mid+short)>long
    }
  }


  def int_if_then_else(b: Boolean, x: Int, y: Int) : Int = {
    // b가 true이면 두 수의 합을
    // b가 false이면 두 수의 차를 구합니다.
    if(b==true){
      x+y
    }else{
      x-y
    }
  }


  def sum_of_fun_val(a:Int, b:Int, c:Int, d:Int, e:Int): Int = {
    // f(d) + f(e) 를 구해줍니다.
  fun_val_sub(a,b,c,d) + fun_val_sub(a,b,c,e)
  }

  def fun_val_sub(a:Int, b:Int, c:Int, x:Int): Int = { //위 함수의 확장버전
    (a*x*x)+(b*x)+c
  }


  def comp3(a:Int, b:Int, c:Int, d:Int): Int = {
    comp3_sub(a,b,c,comp3_sub(a,b,c,comp3_sub(a,b,c,d)))
  }

  def comp3_sub(a:Int, b:Int, c:Int, x:Int): Int = { //위 함수의 확장버전
    (a*x*x)+(b*x)+c
  }


  def string2(s: String): String = {
    // 입력한 문자열 s를 두 번 반복하는 값을 구해줍니다.
    s+s
  }


  def string256(s: String): StringBuilder = {
    val x = new StringBuilder

    for(i<-1 to 256){
      x.append(s)
    }
    x
  }


  def string256_use_string2(s: String): String = {
    string2(string2(string2(string2(string2(string2(string2(string2(s))))))))
  }

}

object hw2_test{
  def main(args: Array[String]): Unit = {
    val p = new hw2

    println("** p1 **")
    println(p.sum(-10))
    println(p.sum(100))


    println("** p2 **")
    println(p.circle(-10.1))
    println(p.circle(4.2))


    println("** p3 **")
    println(p.concat("Bob!"))
    println(p.concat("Alice!"))


    println("** p4 **")
    println(p.xor(true,true))
    println(p.xor(true,false))
    println(p.xor(false,true))
    println(p.xor(false,false))

    println("** p5 **")
    println(p.triangle(-3,3,1))
    println(p.triangle(3,4,5))
    println(p.triangle(100,1,2))

    println("** p6 **")
    println(p.int_if_then_else(true, 2, 100))
    println(p.int_if_then_else((100<2), 2, -2))

    println("** p7 **")
    println(p.sum_of_fun_val(1,2,1,3,4))
    println(p.sum_of_fun_val(1,-3,-1,200,123))

    println("** p8 **")
    println(p.comp3(1,1,1,1))
    println(p.comp3(1,-2,1,3))

    println("** p9 **")
    println(p.string2("hi"))
    println(p.string2("abcde"))

    println("** p10 **")
    println("방법 1")
    println(p.string256("a"))
    println("방법 2")
    println(p.string256_use_string2("a"))

  }

}
