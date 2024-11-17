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
   
   val seq4 = Seq(1, 2, 2, 4)
    
    Console.println(isOrdered(seq4)(_ == _))
  
  val seq = Seq('a','b','a','c','c','a')
  Console.println(freq(seq))
  val seq1 = Seq(("dawid", 2.0), ("marek", 1.0))
  Console.println(median(seq1))
  Console.println(minMax(seq1))
  Console.println(threeNumbers(10))
  */

 val str = "test string"
 Console.println(countChars(str))
 Console.println(swap(Seq(1,2,3,4,5)))
 Console.println(score(Seq(1, 3, 2, 2, 4, 5))(Seq(2, 1, 2, 4, 7, 2)))
 exercise4(100)
}

def exercise4(max: Int): Unit ={
 val linie = io.Source
  .fromResource("ogniem-i-mieczem.txt")
  .getLines.toList
  val text = linie.foldLeft("")(_+_)
  val textLowerCase = text.toLowerCase
  val only_letters_string = textLowerCase.filter( (a => a.isLetter))
  val map_letters = only_letters_string.groupBy(identity).mapValues(_.size)
  val histogramLines = map_letters.toSeq.map { case (char, count) =>
      val barLenght = if (count > max) max else count
      s"$char: ${"*" * barLenght}"
    }
    val histogram = histogramLines.foldLeft("")(_+_+"\n")
    Console.println(histogram)
}

@main
def zad5: Unit = {
 case class Województwo(nazwa: String, min: Int)
 // max ID gminy z województwa w: w.min + 19999
 case class Wynik(
  ID: Int,
  KOALICJA_OBYWATELSKA: Int,
  LEWICA_RAZEM: Int,
  POLEXIT: Int,
  JEDNOŚĆ_NARODU: Int,
  PIS: Int,
  EUROPA_CHRISTI: Int,
  WIOSNA: Int,
  KONFEDERACJA: Int,
  KUKIZ15: Int,
  POLSKA_FAIR_PLAY: Int
 )

 val województwa = List(
      Województwo("dolnośląskie",20000),
      Województwo("kujawsko-pomorskie",40000),
      Województwo("lubelskie",60000),
      Województwo("lubuskie",80000),
      Województwo("łódzkie",100000),
      Województwo("małopolskie",120000),
      Województwo("mazowieckie",140000),
      Województwo("opolskie",160000),
      Województwo("podkarpackie",180000),
      Województwo("podlaskie",200000),
      Województwo("pomorskie",220000),
      Województwo("śląskie",240000),
      Województwo("świętokrzyskie",260000),
      Województwo("warmińsko-mazurskie",280000),
      Województwo("wielkopolskie",300000),
      Województwo("zachodniopomorskie",320000)
    )

 val wyniki = io.Source
  .fromResource("wyniki.csv")
  .getLines
  .toList
  .map(l => {
   l.split(",").toList.map(_.toInt) match {
    case List(a,b,c,d,e,f,g,h,i,j,k) => Wynik(a,b,c,d,e,f,g,h,i,j,k)
    case _ => throw new IllegalArgumentException
   }
 })
 Console.println(wyniki)
}


def score(code: Seq[Int])(move: Seq[Int]): (Int, Int)={
 val blacks = code.zip(move).count { case (c, m) => c == m }  
 val whites = code.intersect(move).length - blacks 
 (blacks, whites)
}


def swap[A](seq: Seq[A]): Seq[A] ={
  seq.grouped(2).flatMap {
    case Seq(a,b) => Seq(b,a)
    case Seq(a) => Seq(a)
  }.toSeq

}

def countChars(str: String): Int ={
  str.distinct.lengthIs

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



