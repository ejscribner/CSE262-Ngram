import scala.io.Source

object Ngram extends App {
  if (args.length == 0) {
    println("No Args Passed")
  }

  if (args.length > 0) {
    val filename = args(0)
    for (line <- Source.fromFile(filename).getLines) {
      println(line);
    }
  }
}