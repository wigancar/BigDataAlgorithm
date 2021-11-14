import scala.collection.mutable.ArrayBuffer

@main def task2() =
  val emptyArray: Array[Int] = Array()
  val myGraph: Array[Seq[(Int, Array[Int])]] = Array(Seq((1, Array(2, 3))), Seq((2, Array(5))), 
    Seq((3, Array(1, 5))), Seq((5, emptyArray.clone())), Seq((6, Array(8))), 
    Seq((7, emptyArray.clone())), Seq((8, Array(1, 2, 3, 5, 6))))
  printGraph(myGraph)
  val myInvGraph = MapReduce(myGraph)
  printGraph(myInvGraph)
  val againMyGraph = MapReduce(myInvGraph)
  printGraph(againMyGraph)

def printGraph(myGraph: Array[Seq[(Int, Array[Int])]]) = 
  println("[")
  for kv <- myGraph do 
    val k = kv(0)(0) 
    val v = kv(0)(1)
    println("("+k.toString+", [" + v.mkString(", ") + "])")
  println("]")

def MapReduce(myGraph: Array[Seq[(Int, Array[Int])]]): Array[Seq[(Int, Array[Int])]] =
  var kvAllPairs: ArrayBuffer[Seq[(Int, Array[Int])]] = ArrayBuffer()
  var allKeys: ArrayBuffer[Int] = ArrayBuffer()
  for chunk <- myGraph do
    allKeys = allKeys.concat(ArrayBuffer(chunk(0)(0)))
    val kvPairs = myMap(chunk)
    kvAllPairs = kvAllPairs.concat(kvPairs)
  var invGraph: Array[Seq[(Int, Array[Int])]] = Array()
  val kvAllPairsArray = kvAllPairs.toArray
  for key <- allKeys do 
    val reducerInput = kvAllPairsArray.filter(el => el(0)(0)==key)
    invGraph = invGraph.concat(myReduce(reducerInput, key))
  return invGraph

def myMap(chunk: Seq[(Int, Array[Int])]): ArrayBuffer[Seq[(Int, Array[Int])]] =
  val key: Int = chunk(0)(0)
  val vals = chunk(0)(1)
  var kvPairs: ArrayBuffer[Seq[(Int, Array[Int])]] = ArrayBuffer()
  for value <- vals do 
    val newSeq: Seq[(Int, Array[Int])] = Seq((value, Array(key)))
    kvPairs = kvPairs.concat(ArrayBuffer(newSeq))
  return kvPairs

def myReduce(reducerInput: Array[Seq[(Int, Array[Int])]], reducerKey: Int): Array[Seq[(Int, Array[Int])]] =
  if 
    reducerInput.length == 0 
  then
    val newSeq: Seq[(Int, Array[Int])] = Seq((reducerKey, Array()))
    return Array(newSeq)
  else
    var reducervalues: Array[Int] = Array()
    for kv <- reducerInput do 
      reducervalues = reducervalues.concat(kv(0)(1))
    val newSeq: Seq[(Int, Array[Int])] = Seq((reducerKey, reducervalues))
    return Array(newSeq)


