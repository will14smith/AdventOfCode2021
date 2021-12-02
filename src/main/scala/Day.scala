import scala.annotation.targetName
import scala.io.Source
import scala.util.Using

abstract class Day {
  private val pattern = """.*Day(\d+).*""".r
  private def fileName = "inputs/day" + pattern.findFirstMatchIn(getClass.getName).get.group(1)

  def input : String = Source.fromFile(fileName).mkString
  def lines : Iterator[String] = Source.fromFile(fileName).getLines

  def run[A, B](name: String, data: A, calc: A => B): Unit = {
    val start = System.nanoTime()
    val result = calc(data)
    val end = System.nanoTime()

    println(s"$name: $result in ${Math.round((end - start) / 100000) / 10} ms")
  }
}