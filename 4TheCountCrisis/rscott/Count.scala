import scala.io.StdIn

object Count {

  def findMissing(arr: Array[Int]): Int = {
    val fe: Int             = arr.head
    val le: Int             = arr.last
    val delta: Int          = (le - fe) / arr.size
    val trueArr: Array[Int] = (fe to le by delta).toArray

    trueArr.filterNot(arr.toSet).head
  }

  def main(args: Array[String]): Unit = {
    val n: Int = StdIn.readInt()
    for (_ <- 1 to n) {
      val line: Array[Int] = StdIn.readLine().split("\\s+").drop(1).map(_.toInt)
      val me: Int          = findMissing(line)

      Console.println(me)
    }
  }

}
