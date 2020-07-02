object Sudoku {
  def isValid(board: Array[Array[Int]]): Boolean = {
    val set = (1 to 9).toSet
    val rowValid = board.map(_.toSet == set).forall(_ == true)
    val columnValid = board.transpose.map(_.toSet == set).forall(_ == true)
    val blockVaild = {
      for {
        b <- 0 until 9
        si = (b % 3) * 3
        sj = (b / 3) * 3
        bs = for (i <- si to si + 2; j <- sj to sj + 2) yield board(i)(j)} yield bs.toSet == set
    }.forall(_ == true)
    rowValid && columnValid && blockVaild
  }
}