import scala.io.StdIn

object Parens {

  sealed trait Op
  case object And extends Op
  case object Or  extends Op
  case object Xor extends Op

  def parens(symbols: Array[Boolean], ops: Array[Op]): Int = {
    val n: Int = symbols.size
    val falses: Array[Array[Int]] = Array.fill(n)(Array.fill(n)(0))
    val trues:  Array[Array[Int]] = Array.fill(n)(Array.fill(n)(0))

    for (i <- 0 to n-1) {
      if (!symbols(i)) {
        falses(i)(i) = 1
      } else {
        trues(i)(i) = 1
      }
    }

    for (gap <- 1 to n-1) {
      for ((i, j) <- (0 to n).zip(gap to n-1)) {
        trues(i)(j)  = 0
        falses(i)(j) = 0

        for (g <- 0 to gap-1) {
          val k: Int = i + g
          val tik: Int = trues(i)(k) + falses(i)(k)
          val tkj: Int = trues(k+1)(j) + falses(k+1)(j)

          ops(k) match {
            case And =>
              trues(i)(j)  += trues(i)(k) * trues(k+1)(j)
              falses(i)(j) += tik*tkj - trues(i)(k)*trues(k+1)(j)
            case Or =>
              falses(i)(j) += falses(i)(k) * falses(k+1)(j)
              trues(i)(j)  += tik*tkj - falses(i)(k)*falses(k+1)(j)
            case Xor =>
              trues(i)(j)  += falses(i)(k)*trues(k+1)(j) + trues(i)(k)*falses(k+1)(j)
              falses(i)(j) += trues(i)(k)*trues(k+1)(j) + falses(k+1)(j)
          }
        }
      }
    }

    trues(0)(n-1)
  }

  def parseSymbol(c: Char): Boolean = c match {
    case 'T' => true
    case 'F' => false
    case _   => ???
  }

  def parseOp(c: Char): Op = c match {
    case '&' => And
    case '|' => Or
    case '^' => Xor
    case _   => ???
  }

  def main(args: Array[String]): Unit = {
    val n: Int = StdIn.readInt()
    for (_ <- 1 to n) {
      val Array(_, symbols, ops) = StdIn.readLine().split("\\s+")
      Console.println(parens(symbols.map(parseSymbol(_)).toArray, ops.map(parseOp(_)).toArray))
    }
  }

}
