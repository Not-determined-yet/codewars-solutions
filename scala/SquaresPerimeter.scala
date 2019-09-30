object SquaresPerimeter {

  def fib(n: Int) = {
    type Matrix = Array[Array[BigInt]]

    def mul(a: Matrix, b: Matrix): Matrix = {
      Array(
        Array(a(0)(0) * b(0)(0) + a(0)(1) * b(1)(0), a(0)(0) * b(0)(1) + a(0)(1) * b(1)(1)),
        Array(a(1)(0) * b(0)(0) + a(1)(1) * b(1)(0), a(1)(0) * b(0)(1) + a(1)(1) * b(1)(1)))
    }

    @scala.annotation.tailrec
    def pow(a: Matrix, b:Matrix, n: Long): Matrix ={
      if (n == 0) b
      else {
        if (n % 2 == 1) {
          pow(mul(a,a), mul(b,a), n/2)
        } else pow(mul(a,a), b, n/2)
      }
    }

    val B: Matrix = Array(Array(1,0),Array(0,1))
    val a: Matrix = Array(Array(1,1),Array(1,0))
    val b: Matrix = Array(Array(0,1), Array(1,-1))
    pow(a, B, n)(1)(0)
  }

  def perimeter(n: BigInt): BigInt = {
    (fib(n.toInt+3) - 1) * 4
  }
}