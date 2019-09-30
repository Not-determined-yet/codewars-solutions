object Snail {


  def snail(xs: List[List[Int]]): List[Int] = {
    if (xs.isEmpty) return List()


    import scala.collection.mutable.ArrayBuffer
    val row = xs.length
    val column = xs.head.length
    val num = row * column

    def valid(x: Int, y: Int): Boolean =
      x >= 0 && x < row && y >= 0 && y < column

    val visit = Array.fill(row, column)(false)
    val turn = Array((0,1), (1,0), (0,-1), (-1,0))
    var res =   ArrayBuffer.empty[Int]

    @scala.annotation.tailrec
    def travel(x: Int, y: Int, curNum: Int, step: Int): Unit = {
      if (curNum < num) {
        visit(x)(y) = true
        res += xs(x)(y)
        val xn = x + turn(step%4)._1
        val yn = y + turn(step%4)._2
        if (valid(xn,yn) && !visit(xn)(yn))
          travel(xn, yn, curNum+1, step)
        else {
          val xn = x + turn((step+1)%4)._1
          val yn = y + turn((step+1)%4)._2
          travel(xn, yn, curNum+1, step +1)
        }
      }
    }

    travel(0, 0, 0, 0)

    res.toList
  }


}