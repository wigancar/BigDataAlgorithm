import scala.io._
import java.util.regex.Pattern

import java.io._


@main def WordCloudGenerator() =
  loop()

def loop(): Int = 
  println("Type:\n\t'-f <filename>.txt' to feed generator with .txt file,\n\t'-e <filename>.csv <number_of_words>' " +
    "to export the data to .csv file\n\t'-p <number_of words>' to create console output for cloud generator\n\t" +
    "'-c' to clean the feeding\n\t'-q' to end the program\n\tor just paste some text to feed generator straight " +
    "from the console.")
  val f = "(-f .*txt)".r
  val e = "(-e .*csv [\\d]+)".r
  val p = "(-p [\\d]+)".r
  var wordsCount = scala.collection.mutable.Map[String, Int]()
  for ln <- io.Source.stdin.getLines do
    ln match
      case "-q" => return 0 // zrobione
      case p(ln) =>  // zrobione
        println("output:")
        val number_of_words = ln.split(" ").toList.last.toInt
        console_output(wordsCount, number_of_words)
        println("end of output")
      case "-c" => // zrobione
        println("Cleaning memory...")
        wordsCount = scala.collection.mutable.Map[String, Int]()
        println("Memory is clear now.")
      case f(ln) => // zrobione
        val filename = ln.split(" ").toList.last
        println("Reading data from " + filename + " file.")
        val newWordsCount: scala.collection.mutable.Map[String, Int] = readTxtFile(filename)
        wordsCount = addMapings(wordsCount, newWordsCount)
        println(filename + " file analyzed.")
      case e(ln) =>
        val args_e = ln.split(" ").toList
        val filename = args_e(1)
        val numberOfWords = args_e.last.toInt
        println("exporting data to " + filename)
        writeToCSV(wordsCount, numberOfWords, filename)
        println("data exported to " + filename)
      case _ => 
        println("processing: " + ln)
        val newWordsCount: scala.collection.mutable.Map[String, Int] = processOneLine(ln)
        wordsCount = addMapings(wordsCount, newWordsCount)
  0
      
def console_output(wordsCount: scala.collection.mutable.Map[String, Int], number_of_words: Int)=
  val sortedOfWordsCount = wordsCount.toSeq.sortWith(_._2 > _._2)
  for (k, v) <- sortedOfWordsCount.slice(0,number_of_words) do println(s"$k, $v")

def readTxtFile(filename: String): scala.collection.mutable.Map[String, Int] =
  // read stopwords from file
  val stopwords = Source.fromFile("stop_words_english.txt", "utf-8").getLines.toList
  // read file
  val lines = Source.fromFile(filename, "utf-8").getLines.toList
  val wordss = Source.fromFile(filename, "utf-8").mkString.split("\\W+").toList
  var words: List[String] = List()
  for w <- wordss
    // remove all the punctuation
    // remove stopwords
    if !stopwords.contains(w.toLowerCase.toString)
    if w!="" 
  do
    words = words ++ List(w.toLowerCase.toString)
  /*for l <- lines do
    // remove all the punctuation
    val wordsLine = l.split("\\W+").toList
    for
      w <- wordsLine
      // remove stopwords
      if !stopwords.contains(w.toLowerCase.toString)
      if w!=""
    do
      words = words ++ List(w.toLowerCase.toString)*/
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

def writeToCSV(wordsCount: scala.collection.mutable.Map[String, Int], number_of_words: Int, filename: String)=
  val sortedOfWordsCount = wordsCount.toSeq.sortWith(_._2 > _._2)
  val wordsCountFile = new PrintWriter(new File(filename))
  wordsCountFile.write("weight,word,color,url\n")
  for (k, v) <- sortedOfWordsCount.slice(0,number_of_words) do wordsCountFile.write(s"$v,$k,,\n")
  wordsCountFile.close

def processOneLine(line: String): scala.collection.mutable.Map[String, Int] =
  // read stopwords from file
  val stopwords = Source.fromFile("stop_words_english.txt", "utf-8").getLines.toList
  var words: List[String] = List()
  // remove all the punctuation
  val wordsLine = line.split("\\W+").toList
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

