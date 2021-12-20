import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Success}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._


class MyGraph(filename: String, noChunks: Int=4):
  var mappedValues: ArrayBuffer[Seq[Int]] = ArrayBuffer()
  def readIntoChunks(k: Int): ArrayBuffer[Array[String]] =
    println("Reading graph from " + filename)
    var resultChunks: ArrayBuffer[Array[String]] = ArrayBuffer()
    val readfile = Source.fromFile(filename, "utf-8").getLines.toArray
    val f: Double = readfile.length/k.toDouble
    var i = 0
    while (i<k) do
      resultChunks = resultChunks.concat(ArrayBuffer(readfile.slice((f*i).ceil.toInt, (f*(i+1)).ceil.toInt)))
      i = i + 1
    resultChunks
  
  val chunks = readIntoChunks(noChunks)
  
  def showChunks() =
    var i = 0
    while (i<chunks.length) do
      println("chunk " + i.toString + ": ")
      for el <- chunks(i) do
        println(el)
      i = i+1
  
  def myMap() =
    var res: ArrayBuffer[Seq[Int]] = ArrayBuffer()
    var futures: ArrayBuffer[Future[ArrayBuffer[Seq[Int]]]] = ArrayBuffer()
    for chunk <- chunks do 
      val f = Future {mapChunk(chunk)}
      futures = futures.concat(ArrayBuffer(f))
    
    val result = Future.reduceLeft(futures.toList){case (acc, someQty) => 
      acc.concat(someQty)
    }
    res = Await.result(result, 1000.seconds)
    mappedValues = res
  
  def mapChunk(chunk:Array[String]): ArrayBuffer[Seq[Int]] =
    var res: ArrayBuffer[Seq[Int]] = ArrayBuffer()
    for el <- chunk do
      val nodes = el.split(" ").toList
      res = res.concat(ArrayBuffer(Seq(nodes(0).toInt, 0, 1)))
      res = res.concat(ArrayBuffer(Seq(nodes(1).toInt, 1, 0)))
    res
  
  def myReduce(noReducers: Int = 6): ArrayBuffer[Seq[Int]] =
    val nodes = mappedValues.map(x => x(0)).distinct
    var allForNode: Map[Int, ArrayBuffer[Seq[Int]]] = Map()
    var futures: ArrayBuffer[Future[ArrayBuffer[Seq[Int]]]] = ArrayBuffer()
    var res: ArrayBuffer[Seq[Int]] = ArrayBuffer()
    
    for node <- nodes do
      val reducerInput = mappedValues.filter(el => el(0)==node)
      allForNode += (node -> reducerInput)
    
    val noEls: Double = nodes.length/noReducers.toDouble
    var i = 0
    while (i<noReducers) do
      val inReducer = allForNode.filter(x => nodes.slice((noEls*i).ceil.toInt, (noEls*(i+1)).ceil.toInt).contains(x._1))
      val f = Future {singleReduce(inReducer)}
      futures = futures.concat(ArrayBuffer(f))
      i = i + 1

    val result = Future.reduceLeft(futures.toList){case (acc, someQty) =>
      acc.concat(someQty)
    }
    res = Await.result(result, 1000.seconds)
    res

  def singleReduce(allForNode: Map[Int, ArrayBuffer[Seq[Int]]]): ArrayBuffer[Seq[Int]] =
    var res: ArrayBuffer[Seq[Int]] = ArrayBuffer()
    for (key, value) <- allForNode do
      var ins = 0
      var outs = 0
      value.foreach( ins += _(1) )
      value.foreach( outs += _(2) )
      res = res.concat(ArrayBuffer(Seq(key, ins, outs)))
    res

@main def task1 =
  val g = MyGraph("graph.txt", 4)
  //g.showChunks()
  val res = g.myMap()
  //println(g.mappedValues)
  //println(g.mappedValues.length)
  val ost_res = g.myReduce(3)
  println(ost_res)
  