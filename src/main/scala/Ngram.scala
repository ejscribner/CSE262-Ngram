import scala.io.Source
import scala.collection.JavaConversions._

object Ngram {
    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println("No Args Passed")
        }

        if (args.length > 0) {
            val filename = args(0)
            var line = Source.fromFile(filename).mkString.toLowerCase
            val words = line.split("\\s").toList
            println(words);
            val pairCounts = createTuple(words)
            println(pairCounts)
        }
    }

    def createTuple(words: List[String]): Map[Tuple2[String, String], Int] = {
        var pairCounts = Map[Tuple2[String, String], Int]()
        var i = 0
        for (i <- 0 to words.length - 2) {
            val currentCount: Int = pairCounts.getOrElse((words(i), words(i + 1)), 0)
            if (pairCounts.exists(_ == (words(i), words(i + 1)) -> currentCount)) {
                val newCount = currentCount + 1
                newCount.toString()
                pairCounts = pairCounts - ((words(i), words(i + 1)))
                pairCounts += (words(i), words(i + 1)) -> newCount
            } else {
                pairCounts += (words(i), words(i + 1)) -> 1
            }


        }

        return pairCounts
    }

    def countAll(word: String): Int = {
        return 0
    }
}

