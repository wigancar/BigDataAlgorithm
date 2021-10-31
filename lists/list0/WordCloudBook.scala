import scala.io.Source

@main def WordCloudBook() =
  // read stopwords from file
  val pathToStopwords = "stop_words_english.txt"
  val stopwords = Source.fromFile(pathToStopwords, "utf-8").getLines.toList
  // read file
  val pathToFile = "1408-0/1408-0.txt"
  val lines = Source.fromFile(pathToFile, "utf-8").getLines.toList
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
  val sortedOfWordsCount = wordsCount.toSeq.sortWith(_._2 > _._2)
  // display some of the most frequently appearing words
  val n = 10
  for (k, v) <- sortedOfWordsCount.slice(0,n) do println(s"$k, $v")