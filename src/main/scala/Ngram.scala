import scala.io.Source
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map => MutableMap}


object Ngram {
    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println("No Args Passed")
        }

        if (args.length > 0) {
            val filename = args(0)
            var line = Source.fromFile(filename).mkString.toLowerCase
            val words = line.split("\\s").toList
            val pairCounts = createTuple(words)
            println(pairCounts)
            //println(mostLikelyNextWord("serious", pairCounts))
        }
    }

    def createTuple(words: List[String]): Map[Tuple2[String, String], Int] = {
        var pairCountsImmutable = Map[Tuple2[String, String], Int]()
        val pairCounts = collection.mutable.Map(pairCountsImmutable.toSeq: _*)
        var i = 0
        for (i <- 0 to words.length - 2) {
            val currentCount: Int = pairCounts.getOrElse((words(i), words(i + 1)), 0)
            if (pairCounts.exists(_ == (words(i), words(i + 1)) -> currentCount)) {
                val newCount = currentCount + 1
                newCount.toString()
                var key = pairCounts(words(i), words(i + 1))
                key = key + 1
                pairCounts((words(i), words(i + 1))) = key

            } else {
                pairCounts += (words(i), words(i + 1)) -> 1
            }
        }
        var pairCountsImmutable2 = collection.immutable.Map(pairCounts.toList: _*)
        return pairCountsImmutable2
    }

    def countAll(word: String, pairCounts: Map[Tuple2[String, String], Int]): Int = {
        var counts = 0
        pairCounts.iterator.foreach(x => {
            if (x._1._1 == word) {
                val currentCounts = x._2
                counts = counts + currentCounts
            }
        })
        return counts
    }

    def p1(of: String, given: String, pairCounts: Map[Tuple2[String, String], Int]): Double = {
        var sum: Double = 0
        pairCounts.iterator.foreach(x => {
            sum += x._2
        })
        var prob: Double = pairCounts.getOrElse((given, of), 0.0).asInstanceOf[Number].doubleValue
        prob = prob / sum
        return prob
    }

    def mostLikelyNextWord(given: String, pairCounts: Map[Tuple2[String, String], Int]): String = {
        var currentCounts = 0
        var word: String = ""
        pairCounts.iterator.foreach(x => {
            if (x._1._1 == given) {
                if (x._2 > currentCounts) {
                    val currentCounts = x._2
                    word = x._1._2
                }
            }
        })
        return word
    }

}

