import java.math.BigInteger
import java.math.BigInteger.ZERO
import java.math.BigInteger.valueOf

object Faberge {
  def C(n: Int, r: Int): BigInteger = {
    var ans = BigInteger.valueOf(1L)
    var res = ZERO
    for (i <- 0 until r) {
      ans = ans.multiply(BigInteger.valueOf(n - i))
      ans = ans.divide(BigInteger.valueOf(i + 1))
      res = res.add(ans)
    }
    res
  }


  def height(n: BigInteger, m: BigInteger): BigInteger = {
    val _n = n.intValue()
    val _m = m.intValue()
    C(_m, _n)
  }
}