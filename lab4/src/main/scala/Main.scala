@main
def mainProg: Unit = {
  /*val sequence1 = Seq(1,2,3,4,5,6,7,8,8,9,9)
  Console.println(subSeq(sequence1, 3,5))
  Console.println(remElems(sequence1, 3))
  val seq1 = Seq(1, 2, 3)
  val seq2 = Seq(2, 2, 1, 3)
  Console.println(diff(seq1, seq2))
  val seq3 = Seq(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
   Console.println(sumOption(seq3))
   Console.println(deStutter(sequence1))
   */
   val seq4 = Seq(1, 2, 2, 4)
    
    Console.println(isOrdered(seq4)(_ == _))
  
  val seq = Seq('a','b','a','c','c','a')
  Console.println(freq(seq))
  val seq1 = Seq(("dawid", 2.0), ("marek", 1.0))
  Console.println(median(seq1))
  Console.println(minMax(seq1))
  Console.println(threeNumbers(10))
}

def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] ={
  val seq2 = seq.drop(begIdx)
  val index2 = endIdx - begIdx
  seq2.take(index2)
}

def remElems[A](seq: Seq[A], k: Int): Seq[A] = {
  seq.zipWithIndex.filter { case (_, index) => index != k }.map(_._1)
}

def diff[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] = {
  //seq1 zip seq2.filter { case (a, b) => a != b }.map(_._1)
  seq1.zip(seq2).filter { case (a, b) => a != b }.map(_._1)
}

def sumOption(seq: Seq[Option[Double]]): Double = {

  seq.foldLeft(0.0) {(accumulator, number) => (accumulator, number) match {
    case (_, Some(value)) => accumulator + value 
    case (_, None) => accumulator
}}
}

def deStutter[A](seq: Seq[A]): Seq[A] = {
  seq.foldRight(Seq.empty[A]) { (char, accumulator) => accumulator match {
    case h::_ if h ==char => accumulator 
    case _               => char +: accumulator
    } 
}
}

def isOrdered[A](seq: Seq[A])(leq:(A, A) => Boolean): Boolean ={
  val sliding_window = seq.sliding(2,1)
  for(group <- sliding_window){
     if (!leq(group(0), group(1))) return false
  }
  true

}

def freq[A](seq: Seq[A]): Set[(A, Int)] = {
   seq.groupBy(identity).map { case (element, occurrences) =>
    (element, occurrences.size)
  }.toSet
}

 def median(seq: Seq[(String, Double)]): Double ={
  val sortedSeq = seq.sortBy(_._2).map(_._2)
  Console.println(sortedSeq)
  val n = sortedSeq.length
  if (n % 2 == 1) {
    sortedSeq(n / 2)
  } else {
    val middle1 = sortedSeq(n / 2 - 1)
    val middle2 = sortedSeq(n / 2)
    (middle1 + middle2) / 2

} }

def minMax(seq: Seq[(String, Double)]): Option[(String, String)] = {
  if (seq.isEmpty) None
  else {
    val minUser = seq.minBy(_._2)._1 
    val maxUser = seq.maxBy(_._2)._1 
    Some((minUser, maxUser))
  }
}

def threeNumbers(n: Int): Set[(Int, Int, Int)] = {
  (for {
    a <- 1 to n
    b <- (a + 1) to n   
    cSquare = a * a + b * b
    c = math.sqrt(cSquare).toInt
    if c <= n && c * c == cSquare
  } yield (a, b, c)).toSet
}



