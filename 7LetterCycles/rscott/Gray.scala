import scala.io.StdIn

object Gray {

  def brgcToBinary(code: Long): Long = {
    var c: Long    = code
    var mask: Long = c >> 1

    while (mask != 0) {
      c ^= mask
      mask >>= 1
    }

    c
  }

  def adjacent(w1: Long, w2: Long, bits: Long): Boolean = {
    val numCodes: Long = 1 << bits
    val ni: Long       = brgcToBinary(w1)
    val mi: Long       = brgcToBinary(w2)

    (ni + 1) % numCodes == mi || (mi + 1) % numCodes == ni
  }

  def parseLetters(ls: String): Long = {
    val bs: String = ls.map({ case 'A' => '0'; case 'B' => '1'; case _ => ??? })
    java.lang.Long.parseLong(bs, 2)
  }

  def main(args: Array[String]): Unit = {
    val n: Int = StdIn.readInt()
    for (_ <- 1 to n) {
      val Array(bits, w1, w2) = StdIn.readLine().split("\\s+")
      val adj: Boolean = adjacent(parseLetters(w1), parseLetters(w2), bits.toLong)
      Console.println(if (adj) "Consecutive" else "Not Consecutive")
    }
  }

}
