import scala.io._
import java.util.regex.Pattern
import scala.math.log

import java.io._


@main def WordCloudGenerator() =
  loop()

def loop(): Int = 
  println("Type:\n\t'-f <filename>.txt' to feed generator with .txt file,\n\t'-p <number_of words>' to create console " +
    "output\n\t'-c' to clean the feeding\n\t'-t <number_of_words>' to calculate and print tfidf\n\t'-q' to end the program")
  val f = "(-f .*txt)".r
  val p = "(-p [\\d]+)".r
  val t = "(-t [\\d]+)".r
  var wordsCount = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]]()
  var wordsCountAll = scala.collection.mutable.Map[String, Int]()
  for ln <- io.Source.stdin.getLines do
    ln match
      case "-q" => return 0  // zrobione
      case p(ln) =>  // zrobione
        val number_of_words = ln.split(" ").toList.last.toInt
        for (text, mapping) <- wordsCount do
          println("output for " + text + ":")
          console_output(mapping, number_of_words)
          println("end of output for " + text + "\n\n")
        println("output for all texts together:")
        console_output(wordsCountAll, number_of_words)
        println("end of output for all texts together\n\n")
      case "-c" =>  // zrobione
        println("Cleaning memory...")
        wordsCount = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]]()
        wordsCountAll = scala.collection.mutable.Map[String, Int]()
        println("Memory is clear now.")
      case f(ln) =>
        val filename = ln.split(" ").toList.last
        if wordsCount.keys.toList.contains(filename) then
          println("This file has been analyzed already.")
        else
          println("Reading data from " + filename + " file.")
          val newWordsCount: scala.collection.mutable.Map[String, Int] = readTxtFile(filename)
          wordsCountAll = addMapings(wordsCountAll, newWordsCount)
          wordsCount += (filename -> newWordsCount)
          println(filename + " file analyzed.")
      case t(ln) =>
        val number_of_words = ln.split(" ").toList.last.toInt
        val tfidf = calcTFIDF(wordsCount, wordsCountAll)
        for (text, mapping) <- tfidf do
          println("tfidf for " + text + ":")
          tfidf_output(mapping, number_of_words)
          println("end of tfidf for " + text + "\n\n")
      case _ => // zrobione
        println("Not valid command: " + ln)
  0
      
def console_output(wordsCount: scala.collection.mutable.Map[String, Int], number_of_words: Int)=
  val sortedOfWordsCount = wordsCount.toSeq.sortWith(_._2 > _._2)
  for (k, v) <- sortedOfWordsCount.slice(0,number_of_words) do println(s"$k, $v")

def tfidf_output(wordsCount: scala.collection.mutable.Map[String, Double], number_of_words: Int)=
  val sortedOfWordsCount = wordsCount.toSeq.sortWith(_._2 > _._2)
  for (k, v) <- sortedOfWordsCount.slice(0,number_of_words) do println(s"$k, $v")

def readTxtFile(filename: String): scala.collection.mutable.Map[String, Int] =
  // read stopwords from file
  val stopwords = Source.fromFile("stop_words_english.txt", "utf-8").getLines.toList
  // read file
  val lines = Source.fromFile(filename, "utf-8").getLines.toList
  var words: List[String] = List()
  for l <- lines do
    // remove all the punctuation
    val wordsLine = l.split("\\W+").toList
    for
      w <- wordsLine
      // remove stopwords
      if !stopwords.contains(w.toLowerCase.toString)
      if w!=""
    do
      words = words ++ List(w.toLowerCase.toString)
  // create the collection of pairs in a form of (word, count)
  val wordsCount = words.groupMapReduce(identity)(_ => 1)(_ + _)
  val wordsCountReturn = collection.mutable.Map(wordsCount.toSeq: _*)
  return wordsCountReturn

def addMapings(wordsCount: collection.mutable.Map[String, Int],
               newWordsCount: collection.mutable.Map[String, Int]): collection.mutable.Map[String, Int] =
  for (k, v) <- newWordsCount do 
    if wordsCount.keys.toList.contains(k) then 
      wordsCount(k) = wordsCount(k) + newWordsCount(k) 
    else 
      wordsCount(k) = newWordsCount(k)
  return wordsCount

def calcTFIDF(wordsCount: scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]],
              wordsCountAll: scala.collection.mutable.Map[String, Int]):
              scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]] = 
  
  var allFilesTFIDF: scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]] = 
    scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]]()
  var IDF: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]()
  val nOfFiles = wordsCount.keys.toList.length
  
  // calculating IDF
  for (k,v) <- wordsCountAll do
    var filesWithK = 0
    for (text, mapping) <- wordsCount do
      if mapping.keys.toList.contains(k) then
        filesWithK = 1 + filesWithK
    IDF += (k -> log(nOfFiles.toDouble/filesWithK.toDouble))

  for (text, mapping) <- wordsCount do
    var mappingTFIDF: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]()
    var TF_denom = mapping.foldLeft(0)(_+_._2)
    for (k,v) <- mapping do
      val tf: Double = v.toDouble/TF_denom.toDouble
      mappingTFIDF += (k -> tf*IDF(k))
    allFilesTFIDF += (text -> mappingTFIDF)
  return allFilesTFIDF



