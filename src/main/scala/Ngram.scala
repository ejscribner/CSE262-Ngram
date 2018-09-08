/*
Elliot Scribner
CSE 262
Prog1: Ngram
 */

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
            var wordsUnique = words.distinct
            wordsUnique.iterator.foreach(x => {
                println(x + " " + mostLikelyNextWord(x, pairCounts))
            })
        }
    }

    def createTuple(words: List[String]): Map[Tuple2[String, String], Int] = {
        val pairs = words.zip(words.drop(1))
        var pairCountsImmutable = Map[Tuple2[String, String], Int]()
        val pairCounts = collection.mutable.Map(pairCountsImmutable.toSeq: _*)
        pairs.map {
            x =>
                if (pairCounts.contains(x)) {
                    pairCounts(x) = pairCounts(x) + 1
                } else {
                    pairCounts += (x -> 1)
                }
        }
        pairCountsImmutable = collection.immutable.Map(pairCounts.toList: _*)
        return pairCountsImmutable
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
                    currentCounts = x._2
                    word = x._1._2
                }
            }
        })
        return word
    }

}

