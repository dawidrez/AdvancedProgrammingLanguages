@main
def mainProg: Unit = {
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
  val groupedResults = wyniki.groupBy(wynik => 
    województwa.find(woj => wynik.ID >= woj.min && wynik.ID <= woj.min + 19999).get.nazwa
  )
    val differences = groupedResults.map { case (wojName, results) =>
    val totalKO = results.map(_.KOALICJA_OBYWATELSKA).sum
    val totalPIS = results.map(_.PIS).sum
    val sumVotes = totalKO + totalPIS
    val diffPercent = if (sumVotes == 0) 0.0 else math.abs(totalKO - totalPIS) * 100.0 / sumVotes
    (wojName, diffPercent)
    }
   val minDifference = differences.minBy(_._2)._2
  val minVoivodeships = differences.filter(_._2 == minDifference).keySet
Console.println(s"Województwa with minimal percentage difference: $minVoivodeships")
  val maxDifference = differences.maxBy(_._2)._2
  val maxVoivodeships = differences.filter(_._2 == maxDifference).keySet
  println(s"Województwa with maximum percentage difference: $maxVoivodeships")

}

@main
def zad6: Unit = {
 case class Ocena(imie: String, nazwisko:String, ocena_wdzięku: Int, ocena_sprytu: Int)
 val oceny = Seq(Ocena(imie="dawid", nazwisko="kowalski", ocena_wdzięku=15, ocena_sprytu=7 ),
 Ocena(imie="dawid", nazwisko="kowalski", ocena_wdzięku=6, ocena_sprytu=4 ),
 Ocena(imie="dawid", nazwisko="stefanski", ocena_wdzięku=5, ocena_sprytu=8 ),Ocena(imie="dawid", nazwisko="stefanski", ocena_wdzięku=6, ocena_sprytu=7 ),
 Ocena(imie="dawid", nazwisko="kowalski", ocena_wdzięku=15, ocena_sprytu=7 ),Ocena(imie="dawid", nazwisko="nowak", ocena_wdzięku=15, ocena_sprytu=9 )
 ,Ocena(imie="dawid", nazwisko="stefanski", ocena_wdzięku=8, ocena_sprytu=10 ),Ocena(imie="dawid", nazwisko="nowak", ocena_wdzięku=18, ocena_sprytu=7 ))

 val zgrupowaneOceny = oceny.groupBy(g => (g.nazwisko, g.imie))
 val średnieOceny = zgrupowaneOceny.map { case ((nazwisko, imie), results) =>
  val średnia_wdzięku = results.map(_.ocena_wdzięku).sum/results.length
  val średnia_sprytu = results.map(_.ocena_sprytu).sum/results.length
  (nazwisko,imie, (średnia_wdzięku+ średnia_sprytu/2), średnia_wdzięku, średnia_sprytu)
 }
val sorted = średnieOceny.toSeq.sortBy { case (nazwisko, _, średniaŁączna, średniaWdzięku, _) =>
  (-średniaŁączna, -średniaWdzięku, nazwisko)
}
sorted.foreach(println)
 

}
